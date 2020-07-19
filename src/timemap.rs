use std::{fmt, marker::PhantomData, str::FromStr};

mod sealed {
    /// Marker Trait for the typestate pattern in a [`RequestBuilder`]
    ///
    /// [`RequestBuilder`]: ../struct.RequestBuilder.html
    pub trait RequestState {}
    pub trait RequestField: super::ToStaticStr + Into<super::AnyField> {}
}

/// The base URL for the timemap of the Internet Archives' Wayback Machine
///
/// i.e. `https://web.archive.org/web/timemap/?`
pub const TIMEMAP_BASE: &'static str = "https://web.archive.org/web/timemap/?";

/// Restriction the returned items **before** collapsing/grouping
pub struct Filter<'a> {
    invert: bool,
    field: Field,
    regex: &'a str,
}

impl<'a> Filter<'a> {
    fn parse_from_str(s: &'a str) -> Result<Self, ParseFilterError> {
        let (invert, input) = if s.starts_with("!") {
            (true, s /*.split_at(1).1*/)
        } else {
            (false, s)
        };

        let mut split = input.splitn(2, ":");
        let field: Field = if let Some(f) = split.next() {
            f.parse().map_err(ParseFilterError::UnknownField)?
        } else {
            panic!("splitn should always return at least one item");
        };

        if let Some(regex) = split.next() {
            Ok(Filter {
                invert,
                field,
                regex,
            })
        } else {
            Err(ParseFilterError::MissingColon)
        }
    }

    pub fn to_owned(&self) -> FilterBuf {
        FilterBuf {
            invert: self.invert,
            field: self.field,
            regex: self.regex.to_owned(),
        }
    }
}

pub struct FilterBuf {
    invert: bool,
    field: Field,
    regex: String,
}

impl FilterBuf {
    pub fn as_ref<'a>(&'a self) -> Filter<'a> {
        Filter {
            invert: self.invert,
            field: self.field,
            regex: &self.regex,
        }
    }
}

#[derive(Debug)]
pub enum ParseFilterError {
    MissingColon,
    UnknownField(UnknownFieldError),
}

impl fmt::Display for ParseFilterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingColon => write!(f, "Missing `:` between field name and regex"),
            Self::UnknownField(e) => write!(f, "{}", e),
        }
    }
}

impl FromStr for FilterBuf {
    type Err = ParseFilterError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Filter::parse_from_str(s).map(|f| f.to_owned())
    }
}

impl<'a> Filter<'a> {
    pub fn new(field: Field, regex: &'a str) -> Self {
        Self {
            invert: false,
            field,
            regex,
        }
    }

    pub fn inverted(field: Field, regex: &'a str) -> Self {
        Self {
            invert: true,
            field,
            regex,
        }
    }
}

/// A configured URL for a request to the wayback machines' timemap
///
/// ```
/// use wayback::timemap::{Request, Field::{Timestamp, StatusCode, UrlKey}};
///
/// let r = Request::builder("nexushq.universe.lego.com/en-us/character/details")
///                 .match_prefix()
///                 .with_field(Timestamp)
///                 .with_field(UrlKey)
///                 .filter_inverted(StatusCode, "[45]..")
///                 .collapse(UrlKey)
///                 .done().to_url();
/// assert_eq!(&r, "https://web.archive.org/web/timemap/\
/// ?url=nexushq.universe.lego.com%2Fen-us%2Fcharacter%2Fdetails\
/// &fl=timestamp,urlkey\
/// &matchType=prefix\
/// &collapse=urlkey\
/// &filter=!statuscode:[45]..");
/// ```
pub struct Request<'a> {
    /// URL
    url: &'a str,
    /// Output format
    output: Output,
    /// Fields
    fl: Vec<AnyField>,
    /// Filter
    filter: Vec<Filter<'a>>,
    /// Match Type
    match_type: MatchType,
    /// Collapse to groups
    collapse: Option<Field>,
}

impl<'a> Request<'a> {
    /// Create a new builder in the basic (non-grouped) state
    pub fn builder(url: &'a str) -> RequestBuilder<'a, BasicRequest> {
        RequestBuilder::new(url)
    }

    /// Return the URL that this request represents as an owned string
    pub fn to_url(&self) -> String {
        let mut url = TIMEMAP_BASE.to_string();
        url.push_str("url=");
        url.push_str(&urlencoding::encode(self.url));
        let mut fl_iter = self.fl.iter();
        if let Some(s) = fl_iter.next() {
            url.push_str("&fl=");
            url.push_str(s.to_static_str());
            for f in fl_iter {
                url.push_str(",");
                url.push_str(f.to_static_str());
            }
        }
        if let Some(match_type) = self.match_type.opt_static_str() {
            url.push_str("&matchType=");
            url.push_str(match_type);
        }
        if let Some(output) = self.output.opt_static_str() {
            url.push_str("&output=");
            url.push_str(output);
        }
        if let Some(collapse) = self.collapse {
            url.push_str("&collapse=");
            url.push_str(collapse.to_static_str());
        }
        let filter_iter = self.filter.iter();
        for filter in filter_iter {
            url.push_str("&filter=");
            if filter.invert {
                url.push_str("!");
            }
            url.push_str(filter.field.to_static_str());
            url.push_str(":");
            url.push_str(filter.regex);
        }
        url
    }
}

/// Struct implementing the builder \& typestate patterns for
/// creating a request
pub struct RequestBuilder<'a, S: sealed::RequestState> {
    /// The request under construction
    request: Request<'a>,
    /// The current state
    _state: PhantomData<S>,
}

/// Marker type for a basic (non-grouped) request
pub struct BasicRequest;
/// Marker type for a grouped (collapsed) request
pub struct GroupedRequest;

impl sealed::RequestState for BasicRequest {}
impl sealed::RequestState for GroupedRequest {}

impl<'a, S: sealed::RequestState> RequestBuilder<'a, S> {
    /// Drop the builder and extract the finished request.
    pub fn done(self) -> Request<'a> {
        self.request
    }

    /// Set the match type of the request
    pub fn match_type(mut self, match_type: MatchType) -> Self {
        self.request.match_type = match_type;
        self
    }

    /// Shorthand for `match_type(MatchType::Prefix)`
    pub fn match_prefix(self) -> Self {
        self.match_type(MatchType::Prefix)
    }

    /// Set the output format of the request
    pub fn output(mut self, output: Output) -> Self {
        self.request.output = output;
        self
    }

    /// Add a filter to be applied to the whole set of entries before any grouping takes place
    ///
    /// Normal filters are requirements, inverted filters exclude matching results.
    pub fn with_filter(mut self, filter: Filter<'a>) -> Self {
        self.request.filter.push(filter);
        self
    }

    /// Shorthand for `with_filter(Filter::new(...))`
    pub fn filter(self, field: Field, regex: &'a str) -> Self {
        self.with_filter(Filter::new(field, regex))
    }

    /// Shorthand for `with_filter(Filter::inverted(...))`
    pub fn filter_inverted(self, field: Field, regex: &'a str) -> Self {
        self.with_filter(Filter::inverted(field, regex))
    }
}

impl<'a> RequestBuilder<'a, BasicRequest> {
    /// Create a new basic request
    pub fn new(url: &'a str) -> Self {
        Self {
            request: Request {
                url,
                output: Output::Default,
                fl: Vec::new(),
                match_type: MatchType::Exact,
                collapse: None,
                filter: Vec::new(),
            },
            _state: PhantomData,
        }
    }

    /// Explicitly add a [`Field`] to the list of fields in the result.
    ///
    /// Fields can be added multiple times. When no field is added
    /// to a request, the server chooses a default set of fields
    ///
    /// [`Field`]: enum.Field.html
    pub fn with_field(mut self, field: Field) -> Self {
        self.request.fl.push(field.into());
        self
    }

    /// Collapse the result set by grouping entries for which the given field
    /// has the same value.
    ///
    /// This is generally used to group results by [`UrlKey`]
    ///
    /// [`UrlKey`]: enum.Field.html#variant.UrlKey
    pub fn collapse(mut self, field: Field) -> RequestBuilder<'a, GroupedRequest> {
        self.request.collapse = Some(field);
        RequestBuilder {
            request: self.request,
            _state: PhantomData,
        }
    }
}

impl<'a> RequestBuilder<'a, GroupedRequest> {
    /// Explicitly add a [`Field`] or [`GroupField`] to the list of fields in the result.
    ///
    /// Fields can be added multiple times. When no field is added to a request,
    /// the server chooses a default set of fields
    ///
    /// [`Field`]: enum.Field.html
    /// [`GroupField`]: enum.GroupField.html
    pub fn with_field<F: sealed::RequestField>(mut self, field: F) -> Self {
        self.request.fl.push(field.into());
        self
    }
}

/// All fields that single entries in the archives' database have
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Field {
    /// The original Location/URL of the memeto
    Original,
    /// The timestamp when the memeto was stored
    Timestamp,
    /// A simplified variant of a URL that represents semantic equality of URLs,
    /// e.g. `http://example.com` and `http://www.example.com:80/` have the same key
    UrlKey,
    /// The content type of teh resource, e.g. `text/html`
    MimeType,
    /// The HTTP status code at crawl time, e.g. `200`, `404`, `302`
    StatusCode,
    /// A Hash Value?
    Digest,
    /// ...
    Redirect,
    /// ...
    RobotFlags,
    /// The length of the memeto
    Length,
    /// The offset of the memeto within containing archive
    Offset,
    /// The filename of a compressed archive that contains the memeto
    Filename,
}

impl sealed::RequestField for Field {}

/// Fields that are available when the result is collapsed/grouped
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GroupField {
    EndTimestamp,
    GroupCount,
    UniqCount,
}

/// All fields that can be set in a request
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AnyField {
    Basic(Field),
    Group(GroupField),
}

impl ToStaticStr for AnyField {
    fn to_static_str(&self) -> &'static str {
        match self {
            Self::Basic(f) => f.to_static_str(),
            Self::Group(f) => f.to_static_str(),
        }
    }
}

impl From<Field> for AnyField {
    fn from(f: Field) -> Self {
        Self::Basic(f)
    }
}

impl From<GroupField> for AnyField {
    fn from(f: GroupField) -> Self {
        Self::Group(f)
    }
}

impl sealed::RequestField for GroupField {}

/// Trait to mark that all possible values have a fixed string representation
pub trait ToStaticStr {
    fn to_static_str(&self) -> &'static str;
}

/// Variant of [`ToStaticStr`] where one variant is the default choice and may be omitted
///
/// [`ToStaticStr`]: trait.ToStaticStr.html
pub trait OptStaticStr {
    fn opt_static_str(&self) -> Option<&'static str>;
}

/// Trait to mark that one possible value is considered the default
pub trait HasDefaultVariant {
    fn is_default(&self) -> bool;
}

impl ToStaticStr for Field {
    fn to_static_str(&self) -> &'static str {
        match self {
            Self::Original => "original",
            Self::Timestamp => "timestamp",
            Self::UrlKey => "urlkey",
            Self::MimeType => "mimetype",
            Self::StatusCode => "statuscode",
            Self::Digest => "digest",
            Self::Redirect => "redirect",
            Self::RobotFlags => "robotflags",
            Self::Length => "length",
            Self::Offset => "offset",
            Self::Filename => "filename",
        }
    }
}

#[derive(Debug)]
pub struct UnknownFieldError(String);

impl fmt::Display for UnknownFieldError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unknown field name `{}`", self.0)
    }
}

impl FromStr for Field {
    type Err = UnknownFieldError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "original" => Ok(Self::Original),
            "timestamp" => Ok(Self::Timestamp),
            "urlkey" => Ok(Self::UrlKey),
            "mimetype" => Ok(Self::MimeType),
            "statuscode" => Ok(Self::StatusCode),
            "digest" => Ok(Self::Digest),
            "redirect" => Ok(Self::Redirect),
            "robotflags" => Ok(Self::RobotFlags),
            "length" => Ok(Self::Length),
            "offset" => Ok(Self::Offset),
            "filename" => Ok(Self::Filename),
            _ => Err(UnknownFieldError(s.to_owned())),
        }
    }
}

impl ToStaticStr for GroupField {
    fn to_static_str(&self) -> &'static str {
        match self {
            Self::EndTimestamp => "endtimestamp",
            Self::GroupCount => "groupcount",
            Self::UniqCount => "uniqcount",
        }
    }
}

/// How to match an entry to the `url` key of a request.
#[derive(Debug, Eq, PartialEq)]
pub enum MatchType {
    /// The result URL is must match the query exactly
    Exact,
    /// The result URL must start with the query
    Prefix,
}

impl Default for MatchType {
    fn default() -> Self {
        Self::Exact
    }
}

impl HasDefaultVariant for MatchType {
    fn is_default(&self) -> bool {
        *self == Self::Exact
    }
}

impl OptStaticStr for MatchType {
    fn opt_static_str(&self) -> Option<&'static str> {
        match self {
            Self::Exact => None,
            Self::Prefix => Some("prefix"),
        }
    }
}

impl ToStaticStr for MatchType {
    fn to_static_str(&self) -> &'static str {
        match self {
            Self::Exact => "exact",
            Self::Prefix => "prefix",
        }
    }
}

/// Different available output formats
#[derive(Debug, Eq, PartialEq)]
pub enum Output {
    /// Default, space-separated, line-based format
    Default,
    /// Array of Arrays of Strings, first inner array are the column names
    Json,
    /// Memeto Link-Format
    Link,
}

impl HasDefaultVariant for Output {
    fn is_default(&self) -> bool {
        *self == Self::Default
    }
}

impl OptStaticStr for Output {
    fn opt_static_str(&self) -> Option<&'static str> {
        match self {
            Self::Default => None,
            Self::Json => Some("json"),
            Self::Link => Some("link"),
        }
    }
}

impl ToStaticStr for Output {
    fn to_static_str(&self) -> &'static str {
        match self {
            Self::Default => "",
            Self::Json => "json",
            Self::Link => "link",
        }
    }
}
