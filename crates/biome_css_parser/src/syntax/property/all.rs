use crate::parser::CssParser;
use crate::syntax::parse_regular_identifier;
use biome_css_syntax::CssSyntaxKind::*;
use biome_css_syntax::T;
use biome_parser::parsed_syntax::ParsedSyntax::Present;
use biome_parser::prelude::*;

use super::parse_any_implicit_property_value;

#[inline]
pub(crate) fn parse_all_property(p: &mut CssParser) -> ParsedSyntax {
    // Assumes the parent has confirmed we're at the `all` identifier.

    let m = p.start();
    parse_regular_identifier(p).ok();
    p.expect(T![:]);

    parse_any_implicit_property_value(p).ok();

    complete_property_value(p, m, CSS_ALL_PROPERTY)
}
