use crate::parser::CssParser;
use crate::syntax::parse_css_auto;
use crate::syntax::parse_regular_number;
use biome_css_syntax::CssSyntaxKind::*;
use biome_parser::prelude::*;

use super::parse_any_implicit_property_value;

/// https://drafts.csswg.org/css2/#z-index
///
/// ```
/// z-index =
///   auto |
///   <integer> |
///   inherit
/// ```
///
/// `inherit` is covered by the `CssWideKeyword` set.
#[inline]
pub(crate) fn parse_z_index_property(p: &mut CssParser) -> ParsedSyntax {
    // Assumes the parent has confirmed we're at the `z-index` identifier.

    parse_explicit_property_value(p, CSS_Z_INDEX_PROPERTY, |m| {
        parse_css_auto(p).or_else(|| parse_regular_number(p))
    })
}
