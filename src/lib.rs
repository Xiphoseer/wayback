//! # Library to query the memetos from the internet archives wayback machine
//!
//! This library contains an implementation of a builder/typestate pattern
//! to generate URLs for use with the Internet Archive's [Wayback Machine].
//!
//! You still need to use an HTTP client of your choice to fetch and parse the
//! page.
//!
//! [Wayback Machine]: http://web.archive.org/
//!
//! ## Usage
//!
//! ```
//! use wayback_urls::timemap::{Request, Field::{Timestamp, StatusCode, UrlKey}};
//!
//! let r = Request::builder("nexushq.universe.lego.com/en-us/character/details")
//!                 .match_prefix()
//!                 .with_field(Timestamp)
//!                 .with_field(UrlKey)
//!                 .filter_inverted(StatusCode, "[45]..")
//!                 .collapse(UrlKey)
//!                 .done().to_url();
//! assert_eq!(&r, "https://web.archive.org/web/timemap/\
//! ?url=nexushq.universe.lego.com%2Fen-us%2Fcharacter%2Fdetails\
//! &fl=timestamp,urlkey\
//! &matchType=prefix\
//! &collapse=urlkey\
//! &filter=!statuscode:[45]..");
//! ```
//!
//! ## Future work (Examples)
//! * <https://web.archive.org/__wb/sparkline?url={}&collection=web&output=json>
//! * <https://web.archive.org/web/timemap/>

pub mod timemap;
