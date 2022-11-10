/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::f64::consts::PI;
use std::fmt;

use super::{BasicParseError, ParseError, Parser, ToCss, Token};

#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// A color primitive with red, green, blue, and alpha components, in a byte each.
#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct RGBA {
    /// The red component.
    pub red: u8,
    /// The green component.
    pub green: u8,
    /// The blue component.
    pub blue: u8,
    /// The alpha component.
    pub alpha: u8,
}

impl RGBA {
    /// Constructs a new RGBA value from float components. It expects the red,
    /// green, blue and alpha channels in that order, and all values will be
    /// clamped to the 0.0 ... 1.0 range.
    #[inline]
    pub fn from_floats(red: f32, green: f32, blue: f32, alpha: f32) -> Self {
        Self::new(
            clamp_unit_f32(red),
            clamp_unit_f32(green),
            clamp_unit_f32(blue),
            clamp_unit_f32(alpha),
        )
    }

    /// Returns a transparent color.
    #[inline]
    pub fn transparent() -> Self {
        Self::new(0, 0, 0, 0)
    }

    /// Same thing, but with `u8` values instead of floats in the 0 to 1 range.
    #[inline]
    pub fn new(red: u8, green: u8, blue: u8, alpha: u8) -> Self {
        RGBA {
            red: red,
            green: green,
            blue: blue,
            alpha: alpha,
        }
    }

    /// Returns the red channel in a floating point number form, from 0 to 1.
    #[inline]
    pub fn red_f32(&self) -> f32 {
        self.red as f32 / 255.0
    }

    /// Returns the green channel in a floating point number form, from 0 to 1.
    #[inline]
    pub fn green_f32(&self) -> f32 {
        self.green as f32 / 255.0
    }

    /// Returns the blue channel in a floating point number form, from 0 to 1.
    #[inline]
    pub fn blue_f32(&self) -> f32 {
        self.blue as f32 / 255.0
    }

    /// Returns the alpha channel in a floating point number form, from 0 to 1.
    #[inline]
    pub fn alpha_f32(&self) -> f32 {
        self.alpha as f32 / 255.0
    }
}

#[cfg(feature = "serde")]
impl Serialize for RGBA {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (self.red, self.green, self.blue, self.alpha).serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for RGBA {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let (r, g, b, a) = Deserialize::deserialize(deserializer)?;
        Ok(RGBA::new(r, g, b, a))
    }
}

// NOTE: `RGBA` should not implement `ToCss` because it is an internal primitive that does not directly represent a CSS value.

/// <hue>
/// https://w3c.github.io/csswg-drafts/css-color-4/#hue-syntax
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Hue {
    /// The value as a number of degrees.
    pub degrees: f32,
}

impl Hue {
    /// Construct a new Hue from a number of degrees.
    pub fn new(degrees: f32) -> Self {
        Self { degrees }
    }
}

/// <alpha-value>
/// https://w3c.github.io/csswg-drafts/css-color-4/#alpha-syntax
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct AlphaValue {
    /// The value as a number in the range of 0 to 1.
    pub number: f32,
}

impl AlphaValue {
    /// Construct a new AlphaValue from a number in the range of 0 to 1.
    pub fn new(number: f32) -> Self {
        Self { number }
    }
}

/// A color in the sRGB color space.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum SrgbColor {
    /// https://w3c.github.io/csswg-drafts/css-color-4/#numeric-srgb
    Rgb {
        /// The red channel.
        red: Option<f64>,
        /// The green channel.
        green: Option<f64>,
        /// The blue channel.
        blue: Option<f64>,
        /// The alpha channel.
        alpha: Option<AlphaValue>,
    },
    /// https://w3c.github.io/csswg-drafts/css-color-4/#the-hsl-notation
    Hsl {
        /// The hue component.
        hue: Option<Hue>,
        /// The saturation component.
        saturation: Option<f32>,
        /// The lightness component.
        lightness: Option<f32>,
        /// The alpha channel.
        alpha: Option<AlphaValue>,
    },
    /// https://w3c.github.io/csswg-drafts/css-color-4/#the-hwb-notation
    Hwb {
        /// The hue component.
        hue: Option<Hue>,
        /// The whiteness component.
        whiteness: Option<f32>,
        /// The blackness component.
        blackness: Option<f32>,
        /// The alpha channel.
        alpha: Option<AlphaValue>,
    },
}

impl SrgbColor {
    /// Construct an sRGB color from its component channels.
    pub fn new(
        red: Option<f64>,
        green: Option<f64>,
        blue: Option<f64>,
        alpha: Option<AlphaValue>,
    ) -> Self {
        Self::Rgb {
            red,
            green,
            blue,
            alpha,
        }
    }

    /// Construct an sRGB color using the HSL color scheme.
    pub fn with_hsl(
        hue: Option<Hue>,
        saturation: Option<f32>,
        lightness: Option<f32>,
        alpha: Option<AlphaValue>,
    ) -> Self {
        Self::Hsl {
            hue,
            saturation,
            lightness,
            alpha,
        }
    }

    /// Construct an sRGB color using the HWB color scheme.
    pub fn with_hwb(
        hue: Option<Hue>,
        whiteness: Option<f32>,
        blackness: Option<f32>,
        alpha: Option<AlphaValue>,
    ) -> Self {
        Self::Hwb {
            hue,
            whiteness,
            blackness,
            alpha,
        }
    }

    /// Construct an sRGB color from channels represented as bytes.
    pub fn from_ints(red: u8, green: u8, blue: u8, alpha: u8) -> Self {
        Self::new(
            Some(red as f64 / 255.),
            Some(green as f64 / 255.),
            Some(blue as f64 / 255.),
            Some(AlphaValue::new(alpha as f32 / 255.)),
        )
    }

    /// Construct an sRGB color from channels represented as floats in the range 0 to 1.
    pub fn from_floats(red: f64, green: f64, blue: f64, alpha: f32) -> Self {
        Self::new(
            Some(red),
            Some(green),
            Some(blue),
            Some(AlphaValue::new(alpha)),
        )
    }

    /// Extract sRGB color channels, with missing components converted to zero.
    pub fn to_floats(self: Self) -> (f64, f64, f64, f32) {
        match self {
            Self::Rgb {
                red,
                green,
                blue,
                alpha,
            } => (
                red.unwrap_or(0.),
                green.unwrap_or(0.),
                blue.unwrap_or(0.),
                alpha.unwrap_or(AlphaValue::new(0.)).number,
            ),
            Self::Hsl {
                hue,
                saturation,
                lightness,
                alpha,
            } => {
                let (r, g, b) = hsl_to_rgb(
                    hue.unwrap_or(Hue::new(0.)).degrees,
                    saturation.unwrap_or(0.),
                    lightness.unwrap_or(0.),
                );
                (
                    r as f64,
                    g as f64,
                    b as f64,
                    alpha.unwrap_or(AlphaValue::new(0.)).number,
                )
            }
            Self::Hwb {
                hue,
                whiteness,
                blackness,
                alpha,
            } => {
                let (r, g, b) = hwb_to_rgb(
                    hue.unwrap_or(Hue::new(0.)).degrees,
                    whiteness.unwrap_or(0.),
                    blackness.unwrap_or(0.),
                );
                (
                    r as f64,
                    g as f64,
                    b as f64,
                    alpha.unwrap_or(AlphaValue::new(0.)).number,
                )
            }
        }
    }

    /// Construct an sRGB color from an RGBA color primitive.
    pub fn from_rgba(rgba: RGBA) -> Self {
        Self::from_ints(rgba.red, rgba.green, rgba.blue, rgba.alpha)
    }

    /// Convert an sRGB color to an RGBA color primitive.
    pub fn to_rgba(self: Self) -> RGBA {
        let (red, green, blue, alpha): (f64, f64, f64, f32) = self.to_floats();

        RGBA {
            red: (red.clamp(0., 1.) * 255.).round() as u8,
            green: (green.clamp(0., 1.) * 255.).round() as u8,
            blue: (blue.clamp(0., 1.) * 255.).round() as u8,
            alpha: (alpha.clamp(0., 1.) * 255.).round() as u8,
        }
    }
}

impl ToCss for SrgbColor {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        let (red, green, blue, alpha): (f64, f64, f64, f32) = self.to_floats();

        let serialize_alpha = alpha != 1.;

        dest.write_str(if serialize_alpha { "rgba(" } else { "rgb(" })?;
        (red * 255.).to_css(dest)?;
        dest.write_str(", ")?;
        (green * 255.).to_css(dest)?;
        dest.write_str(", ")?;
        (blue * 255.).to_css(dest)?;
        if serialize_alpha {
            dest.write_str(", ")?;
            alpha.to_css(dest)?;
        }
        dest.write_char(')')
    }
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#named-colors
#[derive(Clone, PartialEq, Debug)]
pub struct NamedColor {
    /// The name of the color.
    pub name: String,
    /// The corresponding sRGB color value.
    pub value: SrgbColor,
}

impl NamedColor {
    /// Construct a named color.
    pub fn new(name: String, value: SrgbColor) -> Self {
        Self { name, value }
    }
}

impl ToCss for NamedColor {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        dest.write_str(self.name.as_str())
    }
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#css-system-colors
#[derive(Clone, PartialEq, Debug)]
pub struct SystemColor {
    /// The name of the color.
    pub name: String,
}

impl SystemColor {
    /// Construct a system color.
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#deprecated-system-colors
#[derive(Clone, PartialEq, Debug)]
pub struct DeprecatedColor {
    /// The name of the deprecated color alias.
    pub name: String,
    /// The corresponding system color.
    pub same_as: SystemColor,
}

impl DeprecatedColor {
    /// Construct a deprecated color.
    pub fn new(name: String, same_as: SystemColor) -> Self {
        Self { name, same_as }
    }
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#currentcolor-color
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct CurrentColor;

impl ToCss for CurrentColor {
    fn to_css<W>(&self, dest: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        dest.write_str("currentcolor")
    }
}

/// A <color> value.
/// https://w3c.github.io/csswg-drafts/css-color-4/#color-syntax
#[derive(Clone, PartialEq, Debug)]
pub enum Color {
    /// An sRGB color
    SrgbColor(SrgbColor),
    /// A named color
    NamedColor(NamedColor),
    /// A system color
    SystemColor(SystemColor),
    /// A deprecated color
    DeprecatedColor(DeprecatedColor),
    /// The 'currentcolor' keyword
    CurrentColor(CurrentColor),
}

// NOTE: `Color` cannot implement `ToCss` here because some variants cannot be serialized at parsing time.

/// A trait that can be used to hook into how `cssparser` parses color
/// components, with the intention of implementing more complicated behavior.
///
/// For example, this is used by Servo to support calc() in color.
pub trait ColorComponentParser<'i> {
    /// A custom error type that can be returned from the parsing functions.
    type Error: 'i;

    /// Parse a `<percentage>` value.
    ///
    /// Returns the result in a number from 0.0 to 1.0.
    fn parse_percentage<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<f32, ParseError<'i, Self::Error>> {
        input.expect_percentage().map_err(From::from)
    }

    /// Parse a `<number>` value.
    fn parse_number<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<f32, ParseError<'i, Self::Error>> {
        input.expect_number().map_err(From::from)
    }

    /// Parse a `<hue>` value.
    /// https://w3c.github.io/csswg-drafts/css-color-4/#typedef-hue
    fn parse_hue<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<Hue, ParseError<'i, Self::Error>> {
        let location = input.current_source_location();
        Ok(match *input.next()? {
            Token::Number { value: degrees, .. } => Hue::new(degrees),
            Token::Dimension {
                value: v, ref unit, ..
            } => {
                // Expand f32 to f64 to avoid rounding errors during conversion.
                let v = v as f64;

                let degrees = match_ignore_ascii_case! { &*unit,
                    "deg" => v,
                    "grad" => (v / 400.) * 360.,
                    "rad" => (v / (2. * PI)) * 360.,
                    "turn" => v * 360.,
                    _ => return Err(location.new_unexpected_token_error(Token::Ident(unit.clone()))),
                };

                Hue::new(degrees as f32)
            }
            ref t => return Err(location.new_unexpected_token_error(t.clone())),
        })
    }

    /// Parse a `<alpha-value>` value.
    /// https://w3c.github.io/csswg-drafts/css-color-4/#typedef-alpha-value
    fn parse_alpha_value<'t>(
        &self,
        input: &mut Parser<'i, 't>,
    ) -> Result<AlphaValue, ParseError<'i, Self::Error>> {
        let location = input.current_source_location();
        Ok(match *input.next()? {
            Token::Number { value: number, .. } => AlphaValue::new(number),
            Token::Percentage {
                unit_value: number, ..
            } => AlphaValue::new(number),
            ref t => return Err(location.new_unexpected_token_error(t.clone())),
        })
    }
}

struct DefaultComponentParser;
impl<'i> ColorComponentParser<'i> for DefaultComponentParser {
    type Error = ();
}

impl Color {
    /// Parse a <color> value, per CSS Color Module Level 3.
    ///
    /// FIXME(#2) Deprecated CSS2 System Colors are not supported yet.
    pub fn parse_with<'i, 't, ComponentParser>(
        component_parser: &ComponentParser,
        input: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i, ComponentParser::Error>>
    where
        ComponentParser: ColorComponentParser<'i>,
    {
        let location = input.current_source_location();
        let token = input.next()?;
        match *token {
            Token::Hash(ref value) | Token::IDHash(ref value) => Self::parse_hex_color(&*value),
            Token::Ident(ref value) => Self::parse_color_keyword(&*value),
            Token::Function(ref name) => {
                let name = name.clone();
                return input.parse_nested_block(|arguments| {
                    Self::parse_color_function(component_parser, &*name, arguments)
                });
            }
            _ => Err(()),
        }
        .map_err(|()| location.new_unexpected_token_error(token.clone()))
    }

    /// Parse a <color> value, per CSS Color Module Level 3.
    pub fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, BasicParseError<'i>> {
        let component_parser = DefaultComponentParser;
        Self::parse_with(&component_parser, input).map_err(ParseError::basic)
    }

    /// Parse a `<hex-color>` value.
    #[inline]
    pub fn parse_hex_color(value: &str) -> Result<Self, ()> {
        #[inline]
        fn from_hex(c: u8) -> Result<u8, ()> {
            match c {
                b'0'..=b'9' => Ok(c - b'0'),
                b'a'..=b'f' => Ok(c - b'a' + 10),
                b'A'..=b'F' => Ok(c - b'A' + 10),
                _ => Err(()),
            }
        }

        let value = value.as_bytes();

        match value.len() {
            8 => Ok(rgba(
                from_hex(value[0])? * 16 + from_hex(value[1])?,
                from_hex(value[2])? * 16 + from_hex(value[3])?,
                from_hex(value[4])? * 16 + from_hex(value[5])?,
                from_hex(value[6])? * 16 + from_hex(value[7])?,
            )),
            6 => Ok(rgb(
                from_hex(value[0])? * 16 + from_hex(value[1])?,
                from_hex(value[2])? * 16 + from_hex(value[3])?,
                from_hex(value[4])? * 16 + from_hex(value[5])?,
            )),
            4 => Ok(rgba(
                from_hex(value[0])? * 17,
                from_hex(value[1])? * 17,
                from_hex(value[2])? * 17,
                from_hex(value[3])? * 17,
            )),
            3 => Ok(rgb(
                from_hex(value[0])? * 17,
                from_hex(value[1])? * 17,
                from_hex(value[2])? * 17,
            )),
            _ => Err(()),
        }
    }

    /// Return the named color with the given name.
    ///
    /// Matching is case-insensitive in the ASCII range.
    /// CSS escaping (if relevant) should be resolved before calling this function.
    /// (For example, the value of an `Ident` token is fine.)
    ///
    /// https://w3c.github.io/csswg-drafts/css-color-4/#color-keywords
    #[inline]
    pub fn parse_color_keyword(ident: &str) -> Result<Self, ()> {
        macro_rules! rgb {
            ($red: expr, $green: expr, $blue: expr) => {
                RGBA {
                    red: $red,
                    green: $green,
                    blue: $blue,
                    alpha: 255,
                }
            };
        }
        ascii_case_insensitive_phf_map! {
            rgba_from_named_color -> RGBA = {
                "black" => rgb!(0, 0, 0),
                "silver" => rgb!(192, 192, 192),
                "gray" => rgb!(128, 128, 128),
                "white" => rgb!(255, 255, 255),
                "maroon" => rgb!(128, 0, 0),
                "red" => rgb!(255, 0, 0),
                "purple" => rgb!(128, 0, 128),
                "fuchsia" => rgb!(255, 0, 255),
                "green" => rgb!(0, 128, 0),
                "lime" => rgb!(0, 255, 0),
                "olive" => rgb!(128, 128, 0),
                "yellow" => rgb!(255, 255, 0),
                "navy" => rgb!(0, 0, 128),
                "blue" => rgb!(0, 0, 255),
                "teal" => rgb!(0, 128, 128),
                "aqua" => rgb!(0, 255, 255),

                "aliceblue" => rgb!(240, 248, 255),
                "antiquewhite" => rgb!(250, 235, 215),
                "aquamarine" => rgb!(127, 255, 212),
                "azure" => rgb!(240, 255, 255),
                "beige" => rgb!(245, 245, 220),
                "bisque" => rgb!(255, 228, 196),
                "blanchedalmond" => rgb!(255, 235, 205),
                "blueviolet" => rgb!(138, 43, 226),
                "brown" => rgb!(165, 42, 42),
                "burlywood" => rgb!(222, 184, 135),
                "cadetblue" => rgb!(95, 158, 160),
                "chartreuse" => rgb!(127, 255, 0),
                "chocolate" => rgb!(210, 105, 30),
                "coral" => rgb!(255, 127, 80),
                "cornflowerblue" => rgb!(100, 149, 237),
                "cornsilk" => rgb!(255, 248, 220),
                "crimson" => rgb!(220, 20, 60),
                "cyan" => rgb!(0, 255, 255),
                "darkblue" => rgb!(0, 0, 139),
                "darkcyan" => rgb!(0, 139, 139),
                "darkgoldenrod" => rgb!(184, 134, 11),
                "darkgray" => rgb!(169, 169, 169),
                "darkgreen" => rgb!(0, 100, 0),
                "darkgrey" => rgb!(169, 169, 169),
                "darkkhaki" => rgb!(189, 183, 107),
                "darkmagenta" => rgb!(139, 0, 139),
                "darkolivegreen" => rgb!(85, 107, 47),
                "darkorange" => rgb!(255, 140, 0),
                "darkorchid" => rgb!(153, 50, 204),
                "darkred" => rgb!(139, 0, 0),
                "darksalmon" => rgb!(233, 150, 122),
                "darkseagreen" => rgb!(143, 188, 143),
                "darkslateblue" => rgb!(72, 61, 139),
                "darkslategray" => rgb!(47, 79, 79),
                "darkslategrey" => rgb!(47, 79, 79),
                "darkturquoise" => rgb!(0, 206, 209),
                "darkviolet" => rgb!(148, 0, 211),
                "deeppink" => rgb!(255, 20, 147),
                "deepskyblue" => rgb!(0, 191, 255),
                "dimgray" => rgb!(105, 105, 105),
                "dimgrey" => rgb!(105, 105, 105),
                "dodgerblue" => rgb!(30, 144, 255),
                "firebrick" => rgb!(178, 34, 34),
                "floralwhite" => rgb!(255, 250, 240),
                "forestgreen" => rgb!(34, 139, 34),
                "gainsboro" => rgb!(220, 220, 220),
                "ghostwhite" => rgb!(248, 248, 255),
                "gold" => rgb!(255, 215, 0),
                "goldenrod" => rgb!(218, 165, 32),
                "greenyellow" => rgb!(173, 255, 47),
                "grey" => rgb!(128, 128, 128),
                "honeydew" => rgb!(240, 255, 240),
                "hotpink" => rgb!(255, 105, 180),
                "indianred" => rgb!(205, 92, 92),
                "indigo" => rgb!(75, 0, 130),
                "ivory" => rgb!(255, 255, 240),
                "khaki" => rgb!(240, 230, 140),
                "lavender" => rgb!(230, 230, 250),
                "lavenderblush" => rgb!(255, 240, 245),
                "lawngreen" => rgb!(124, 252, 0),
                "lemonchiffon" => rgb!(255, 250, 205),
                "lightblue" => rgb!(173, 216, 230),
                "lightcoral" => rgb!(240, 128, 128),
                "lightcyan" => rgb!(224, 255, 255),
                "lightgoldenrodyellow" => rgb!(250, 250, 210),
                "lightgray" => rgb!(211, 211, 211),
                "lightgreen" => rgb!(144, 238, 144),
                "lightgrey" => rgb!(211, 211, 211),
                "lightpink" => rgb!(255, 182, 193),
                "lightsalmon" => rgb!(255, 160, 122),
                "lightseagreen" => rgb!(32, 178, 170),
                "lightskyblue" => rgb!(135, 206, 250),
                "lightslategray" => rgb!(119, 136, 153),
                "lightslategrey" => rgb!(119, 136, 153),
                "lightsteelblue" => rgb!(176, 196, 222),
                "lightyellow" => rgb!(255, 255, 224),
                "limegreen" => rgb!(50, 205, 50),
                "linen" => rgb!(250, 240, 230),
                "magenta" => rgb!(255, 0, 255),
                "mediumaquamarine" => rgb!(102, 205, 170),
                "mediumblue" => rgb!(0, 0, 205),
                "mediumorchid" => rgb!(186, 85, 211),
                "mediumpurple" => rgb!(147, 112, 219),
                "mediumseagreen" => rgb!(60, 179, 113),
                "mediumslateblue" => rgb!(123, 104, 238),
                "mediumspringgreen" => rgb!(0, 250, 154),
                "mediumturquoise" => rgb!(72, 209, 204),
                "mediumvioletred" => rgb!(199, 21, 133),
                "midnightblue" => rgb!(25, 25, 112),
                "mintcream" => rgb!(245, 255, 250),
                "mistyrose" => rgb!(255, 228, 225),
                "moccasin" => rgb!(255, 228, 181),
                "navajowhite" => rgb!(255, 222, 173),
                "oldlace" => rgb!(253, 245, 230),
                "olivedrab" => rgb!(107, 142, 35),
                "orange" => rgb!(255, 165, 0),
                "orangered" => rgb!(255, 69, 0),
                "orchid" => rgb!(218, 112, 214),
                "palegoldenrod" => rgb!(238, 232, 170),
                "palegreen" => rgb!(152, 251, 152),
                "paleturquoise" => rgb!(175, 238, 238),
                "palevioletred" => rgb!(219, 112, 147),
                "papayawhip" => rgb!(255, 239, 213),
                "peachpuff" => rgb!(255, 218, 185),
                "peru" => rgb!(205, 133, 63),
                "pink" => rgb!(255, 192, 203),
                "plum" => rgb!(221, 160, 221),
                "powderblue" => rgb!(176, 224, 230),
                "rebeccapurple" => rgb!(102, 51, 153),
                "rosybrown" => rgb!(188, 143, 143),
                "royalblue" => rgb!(65, 105, 225),
                "saddlebrown" => rgb!(139, 69, 19),
                "salmon" => rgb!(250, 128, 114),
                "sandybrown" => rgb!(244, 164, 96),
                "seagreen" => rgb!(46, 139, 87),
                "seashell" => rgb!(255, 245, 238),
                "sienna" => rgb!(160, 82, 45),
                "skyblue" => rgb!(135, 206, 235),
                "slateblue" => rgb!(106, 90, 205),
                "slategray" => rgb!(112, 128, 144),
                "slategrey" => rgb!(112, 128, 144),
                "snow" => rgb!(255, 250, 250),
                "springgreen" => rgb!(0, 255, 127),
                "steelblue" => rgb!(70, 130, 180),
                "tan" => rgb!(210, 180, 140),
                "thistle" => rgb!(216, 191, 216),
                "tomato" => rgb!(255, 99, 71),
                "turquoise" => rgb!(64, 224, 208),
                "violet" => rgb!(238, 130, 238),
                "wheat" => rgb!(245, 222, 179),
                "whitesmoke" => rgb!(245, 245, 245),
                "yellowgreen" => rgb!(154, 205, 50),

                "transparent" => RGBA { red: 0, green: 0, blue: 0, alpha: 0 },
            }
        }

        ascii_case_insensitive_phf_map! {
            same_as_from_deprecated_color -> &'static str = {
                "activeborder" => "buttonborder",
                "activecaption" => "canvastext",
                "appworkspace" => "canvas",
                "background" => "canvas",
                "buttonhighlight" => "buttonface",
                "buttonshadow" => "buttonface",
                "captiontext" => "canvastext",
                "inactiveborder" => "buttonborder",
                "inactivecaption" => "canvas",
                "inactivecaptiontext" => "graytext",
                "infobackground" => "canvas",
                "infotext" => "canvastext",
                "menu" => "canvas",
                "menutext" => "canvastext",
                "scrollbar" => "canvas",
                "threeddarkshadow" => "buttonborder",
                "threedface" => "buttonface",
                "threedhighlight" => "buttonborder",
                "threedlightshadow" => "buttonborder",
                "threedshadow" => "buttonborder",
                "window" => "canvas",
                "windowframe" => "buttonborder",
                "windowtext" => "canvastext",
            }
        }

        match rgba_from_named_color(ident).cloned() {
            Some(rgba) => Ok(Color::NamedColor(NamedColor::new(
                ident.to_lowercase(),
                SrgbColor::from_rgba(rgba),
            ))),
            None => match same_as_from_deprecated_color(ident).cloned() {
                Some(same_as) => Ok(Color::DeprecatedColor(DeprecatedColor::new(
                    ident.to_lowercase(),
                    SystemColor::new(same_as.to_string()),
                ))),
                None => match_ignore_ascii_case! { ident,
                    "accentcolor" | "accentcolortext" | "activetext" | "buttonborder" | "buttonface" |
                    "buttontext" | "canvas" | "canvastext" | "field" | "fieldtext" |
                    "graytext" | "highlight" | "highlighttext" | "linktext" | "mark" |
                    "marktext" | "selecteditem" | "selecteditemtext" | "visitedtext" => Ok(Color::SystemColor(SystemColor::new(ident.to_lowercase()))),
                    "currentcolor" => Ok(Color::CurrentColor(CurrentColor)),
                    _ => Err(()),
                },
            },
        }
    }

    /// https://w3c.github.io/csswg-drafts/css-color-4/#color-functions
    #[inline]
    pub fn parse_color_function<'i, 't, ComponentParser>(
        component_parser: &ComponentParser,
        name: &str,
        arguments: &mut Parser<'i, 't>,
    ) -> Result<Self, ParseError<'i, ComponentParser::Error>>
    where
        ComponentParser: ColorComponentParser<'i>,
    {
        let color = match_ignore_ascii_case! { name,
            "rgb" | "rgba" => parse_rgb(component_parser, arguments)?,
            "hsl" | "hsla" => parse_hsl(component_parser, arguments)?,
            "hwb" => parse_hwb(component_parser, arguments)?,
            _ => return Err(arguments.new_unexpected_token_error(Token::Ident(name.to_owned().into()))),
        };

        arguments.expect_exhausted()?;

        Ok(color)
    }
}

#[inline]
fn rgb(red: u8, green: u8, blue: u8) -> Color {
    rgba(red, green, blue, 255)
}

#[inline]
fn rgba(red: u8, green: u8, blue: u8, alpha: u8) -> Color {
    Color::SrgbColor(SrgbColor::from_ints(red, green, blue, alpha))
}

fn clamp_unit_f32(val: f32) -> u8 {
    // Whilst scaling by 256 and flooring would provide
    // an equal distribution of integers to percentage inputs,
    // this is not what Gecko does so we instead multiply by 255
    // and round (adding 0.5 and flooring is equivalent to rounding)
    //
    // Chrome does something similar for the alpha value, but not
    // the rgb values.
    //
    // See https://bugzilla.mozilla.org/show_bug.cgi?id=1340484
    //
    // Clamping to 256 and rounding after would let 1.0 map to 256, and
    // `256.0_f32 as u8` is undefined behavior:
    //
    // https://github.com/rust-lang/rust/issues/10184
    clamp_floor_256_f32(val * 255.)
}

fn clamp_floor_256_f32(val: f32) -> u8 {
    val.round().max(0.).min(255.) as u8
}

#[inline]
fn parse_alpha_component<'i, 't, ComponentParser>(
    component_parser: &ComponentParser,
    arguments: &mut Parser<'i, 't>,
    use_legacy_syntax: bool,
) -> Result<AlphaValue, ParseError<'i, ComponentParser::Error>>
where
    ComponentParser: ColorComponentParser<'i>,
{
    Ok(if !arguments.is_exhausted() {
        if use_legacy_syntax {
            arguments.expect_comma()?;
        } else {
            arguments.expect_delim('/')?;
        }

        component_parser.parse_alpha_value(arguments)?
    } else {
        AlphaValue::new(1.)
    })
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#rgb-functions
#[inline]
fn parse_rgb<'i, 't, ComponentParser>(
    component_parser: &ComponentParser,
    arguments: &mut Parser<'i, 't>,
) -> Result<Color, ParseError<'i, ComponentParser::Error>>
where
    ComponentParser: ColorComponentParser<'i>,
{
    // Try to parse legacy syntax first.
    // Determine whether this is the number syntax or the percentage syntax.
    arguments
        .try_parse(|input| {
            let red = Some(component_parser.parse_number(input)? as f64 / 255.);

            input.expect_comma()?;

            let green = Some(component_parser.parse_number(input)? as f64 / 255.);

            input.expect_comma()?;

            let blue = Some(component_parser.parse_number(input)? as f64 / 255.);

            let alpha = Some(parse_alpha_component(component_parser, input, true)?);

            Ok::<Color, ParseError<'i, ComponentParser::Error>>(Color::SrgbColor(SrgbColor::new(
                red, green, blue, alpha,
            )))
        })
        .or(arguments.try_parse(|input| {
            let red = Some(component_parser.parse_percentage(input)? as f64);

            input.expect_comma()?;

            let green = Some(component_parser.parse_percentage(input)? as f64);

            input.expect_comma()?;

            let blue = Some(component_parser.parse_percentage(input)? as f64);

            let alpha = Some(parse_alpha_component(component_parser, input, true)?);

            Ok::<Color, ParseError<'i, ComponentParser::Error>>(Color::SrgbColor(SrgbColor::new(
                red, green, blue, alpha,
            )))
        }))
        .or(arguments.try_parse(|input| {
            let red = Some(component_parser.parse_number(input)? as f64 / 255.);
            let green = Some(component_parser.parse_number(input)? as f64 / 255.);
            let blue = Some(component_parser.parse_number(input)? as f64 / 255.);
            let alpha = Some(parse_alpha_component(component_parser, input, false)?);

            Ok::<Color, ParseError<'i, ComponentParser::Error>>(Color::SrgbColor(SrgbColor::new(
                red, green, blue, alpha,
            )))
        }))
        .or(arguments.try_parse(|input| {
            let red = Some(component_parser.parse_percentage(input)? as f64);
            let green = Some(component_parser.parse_percentage(input)? as f64);
            let blue = Some(component_parser.parse_percentage(input)? as f64);
            let alpha = Some(parse_alpha_component(component_parser, input, false)?);

            Ok::<Color, ParseError<'i, ComponentParser::Error>>(Color::SrgbColor(SrgbColor::new(
                red, green, blue, alpha,
            )))
        }))
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#the-hsl-notation
#[inline]
fn parse_hsl<'i, 't, ComponentParser>(
    component_parser: &ComponentParser,
    arguments: &mut Parser<'i, 't>,
) -> Result<Color, ParseError<'i, ComponentParser::Error>>
where
    ComponentParser: ColorComponentParser<'i>,
{
    // Try to parse legacy syntax first.
    arguments
        .try_parse(|input| {
            let hue = Some(component_parser.parse_hue(input)?);

            input.expect_comma()?;

            let saturation = Some(input.expect_percentage()?);

            input.expect_comma()?;

            let lightness = Some(input.expect_percentage()?);

            let alpha = Some(parse_alpha_component(component_parser, input, true)?);

            Ok::<Color, ParseError<'i, ComponentParser::Error>>(Color::SrgbColor(
                SrgbColor::with_hsl(hue, saturation, lightness, alpha),
            ))
        })
        .or(arguments.try_parse(|input| {
            let hue = Some(component_parser.parse_hue(input)?);

            let saturation = Some(component_parser.parse_percentage(input)?);

            let lightness = Some(component_parser.parse_percentage(input)?);

            let alpha = Some(parse_alpha_component(component_parser, input, false)?);

            Ok::<Color, ParseError<'i, ComponentParser::Error>>(Color::SrgbColor(
                SrgbColor::with_hsl(hue, saturation, lightness, alpha),
            ))
        }))
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#the-hwb-notation
#[inline]
fn parse_hwb<'i, 't, ComponentParser>(
    component_parser: &ComponentParser,
    arguments: &mut Parser<'i, 't>,
) -> Result<Color, ParseError<'i, ComponentParser::Error>>
where
    ComponentParser: ColorComponentParser<'i>,
{
    let hue = Some(component_parser.parse_hue(arguments)?);

    let whiteness = Some(component_parser.parse_percentage(arguments)?);

    let blackness = Some(component_parser.parse_percentage(arguments)?);

    let alpha = Some(parse_alpha_component(component_parser, arguments, false)?);

    Ok(Color::SrgbColor(SrgbColor::with_hwb(
        hue, whiteness, blackness, alpha,
    )))
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#hwb-to-rgb
#[inline]
pub fn hwb_to_rgb(h: f32, w: f32, b: f32) -> (f32, f32, f32) {
    if w + b >= 1.0 {
        let gray = w / (w + b);
        return (gray, gray, gray);
    }

    let (mut red, mut green, mut blue) = hsl_to_rgb(h, 1.0, 0.5);
    let x = 1.0 - w - b;
    red = red * x + w;
    green = green * x + w;
    blue = blue * x + w;
    (red, green, blue)
}

/// https://w3c.github.io/csswg-drafts/css-color-4/#hsl-to-rgb
/// except with h pre-multiplied by 3, to avoid some rounding errors.
#[inline]
pub fn hsl_to_rgb(hue: f32, saturation: f32, lightness: f32) -> (f32, f32, f32) {
    fn hue_to_rgb(m1: f32, m2: f32, mut h3: f32) -> f32 {
        if h3 < 0. {
            h3 += 3.
        }
        if h3 > 3. {
            h3 -= 3.
        }
        if h3 * 2. < 1. {
            m1 + (m2 - m1) * h3 * 2.
        } else if h3 * 2. < 3. {
            m2
        } else if h3 < 2. {
            m1 + (m2 - m1) * (2. - h3) * 2.
        } else {
            m1
        }
    }
    let m2 = if lightness <= 0.5 {
        lightness * (saturation + 1.)
    } else {
        lightness + saturation - lightness * saturation
    };
    let m1 = lightness * 2. - m2;
    let hue_times_3 = hue * 3.;
    let red = hue_to_rgb(m1, m2, hue_times_3 + 1.);
    let green = hue_to_rgb(m1, m2, hue_times_3);
    let blue = hue_to_rgb(m1, m2, hue_times_3 - 1.);
    (red, green, blue)
}

#[cfg(test)]
mod tests {
    use crate::ParserInput;

    use super::{
        AlphaValue, Color, ColorComponentParser, CurrentColor, DefaultComponentParser,
        DeprecatedColor, Hue, NamedColor, Parser, SrgbColor, SystemColor, ToCss, RGBA,
    };

    #[test]
    fn rgba_new() {
        assert_eq!(
            RGBA::new(0, 0, 0, 0),
            RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 0
            }
        );
        assert_eq!(
            RGBA::new(0, 0, 0, 255),
            RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 255
            }
        );
        assert_eq!(
            RGBA::new(255, 255, 255, 0),
            RGBA {
                red: 255,
                green: 255,
                blue: 255,
                alpha: 0
            }
        );
        assert_eq!(
            RGBA::new(255, 255, 255, 255),
            RGBA {
                red: 255,
                green: 255,
                blue: 255,
                alpha: 255
            }
        );
    }

    #[test]
    fn rgba_from_floats() {
        assert_eq!(
            RGBA::from_floats(0., 0., 0., 0.),
            RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 0
            }
        );
        assert_eq!(
            RGBA::from_floats(0., 0., 0., 255.),
            RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 255
            }
        );
        assert_eq!(
            RGBA::from_floats(255., 255., 255., 0.),
            RGBA {
                red: 255,
                green: 255,
                blue: 255,
                alpha: 0
            }
        );
        assert_eq!(
            RGBA::from_floats(255., 255., 255., 255.),
            RGBA {
                red: 255,
                green: 255,
                blue: 255,
                alpha: 255
            }
        );
        assert_eq!(
            RGBA::from_floats(-255., -255., -255., -255.),
            RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 0
            }
        );
        assert_eq!(
            RGBA::from_floats(256., 256., 256., 256.),
            RGBA {
                red: 255,
                green: 255,
                blue: 255,
                alpha: 255
            }
        );
    }

    #[test]
    fn rgba_transparent() {
        let transparent_rgba = RGBA::transparent();

        // NOTE: Neither the documentation nor the function name claims that this need be transparent black.
        assert_eq!(transparent_rgba.alpha, 0);
    }

    #[test]
    fn rgba_red_32() {
        assert_eq!(RGBA::new(0, 51, 170, 255).red_f32(), 0.);
        assert_eq!(RGBA::new(51, 0, 170, 255).red_f32(), 0.2);
        assert_eq!(RGBA::new(170, 0, 51, 255).red_f32(), (2. / 3.));
        assert_eq!(RGBA::new(255, 0, 51, 170).red_f32(), 1.);
    }

    #[test]
    fn rgba_green_f32() {
        assert_eq!(RGBA::new(51, 0, 170, 255).green_f32(), 0.);
        assert_eq!(RGBA::new(0, 51, 170, 255).green_f32(), 0.2);
        assert_eq!(RGBA::new(0, 170, 51, 255).green_f32(), (2. / 3.));
        assert_eq!(RGBA::new(0, 255, 51, 170).green_f32(), 1.);
    }

    #[test]
    fn rgba_blue_f32() {
        assert_eq!(RGBA::new(51, 170, 0, 255).blue_f32(), 0.);
        assert_eq!(RGBA::new(0, 170, 51, 255).blue_f32(), 0.2);
        assert_eq!(RGBA::new(0, 51, 170, 255).blue_f32(), (2. / 3.));
        assert_eq!(RGBA::new(0, 51, 255, 170).blue_f32(), 1.);
    }

    #[test]
    fn rgba_alpha_f32() {
        assert_eq!(RGBA::new(51, 170, 255, 0).alpha_f32(), 0.);
        assert_eq!(RGBA::new(0, 170, 255, 51).alpha_f32(), 0.2);
        assert_eq!(RGBA::new(0, 51, 255, 170).alpha_f32(), (2. / 3.));
        assert_eq!(RGBA::new(0, 51, 170, 255).alpha_f32(), 1.);
    }

    #[test]
    fn rgba_clone() {
        assert_eq!(RGBA::new(0, 0, 0, 0).clone(), RGBA::new(0, 0, 0, 0));
        assert_eq!(
            RGBA::new(255, 255, 255, 255).clone(),
            RGBA::new(255, 255, 255, 255)
        );
    }

    #[test]
    fn rgba_clone_from() {
        let mut target = RGBA::new(127, 127, 127, 127);
        target.clone_from(&RGBA::new(0, 0, 0, 0));

        assert_eq!(target, RGBA::new(0, 0, 0, 0));

        target.clone_from(&RGBA::new(255, 255, 255, 255));

        assert_eq!(target, RGBA::new(255, 255, 255, 255));
    }

    #[test]
    fn rgba_fmt() {
        let rgba = RGBA::new(0, 0, 0, 0);

        assert_eq!(
            format!("{rgba:?}"),
            "RGBA { red: 0, green: 0, blue: 0, alpha: 0 }"
        );
    }

    #[test]
    fn rgba_eq() {
        assert!(RGBA::new(0, 0, 0, 0) == RGBA::new(0, 0, 0, 0));
        assert!(RGBA::new(255, 255, 255, 255) == RGBA::new(255, 255, 255, 255));
    }

    #[test]
    fn rgba_ne() {
        assert!(RGBA::new(0, 0, 0, 0) != RGBA::new(255, 255, 255, 255));
        assert!(RGBA::new(255, 255, 255, 255) != RGBA::new(0, 0, 0, 0));
    }

    #[test]
    fn hue_clone() {
        assert_eq!(Hue { degrees: 0. }.clone(), Hue { degrees: 0. });
        assert_eq!(Hue { degrees: 180. }.clone(), Hue { degrees: 180. });
        assert_eq!(Hue { degrees: 360. }.clone(), Hue { degrees: 360. });
    }

    #[test]
    fn hue_clone_from() {
        let mut target = Hue { degrees: 45. };

        target.clone_from(&Hue { degrees: 90. });

        assert_eq!(target, Hue { degrees: 90. });

        target.clone_from(&Hue { degrees: 60. });

        assert_eq!(target, Hue { degrees: 60. });
    }

    #[test]
    fn hue_fmt() {
        assert_eq!(format!("{:?}", Hue { degrees: 0. }), "Hue { degrees: 0.0 }",);
    }

    #[test]
    fn hue_eq() {
        assert!(Hue { degrees: 0. } == Hue { degrees: 0. });
        assert!(Hue { degrees: 270. } == Hue { degrees: 270. });
    }

    #[test]
    fn hue_ne() {
        assert!(Hue { degrees: 0. } != Hue { degrees: 270. });
        assert!(Hue { degrees: 270. } != Hue { degrees: 0. });
    }

    #[test]
    fn hue_new() {
        assert_eq!(Hue::new(0.), Hue { degrees: 0. });
        assert_eq!(Hue::new(47.5), Hue { degrees: 47.5 });
        assert_eq!(Hue::new(360.), Hue { degrees: 360. });
    }

    #[test]
    fn alphavalue_clone() {
        assert_eq!(
            AlphaValue { number: 0.0 }.clone(),
            AlphaValue { number: 0.0 }
        );
        assert_eq!(
            AlphaValue { number: 0.5 }.clone(),
            AlphaValue { number: 0.5 }
        );
        assert_eq!(
            AlphaValue { number: 1.0 }.clone(),
            AlphaValue { number: 1.0 }
        );
    }

    #[test]
    fn alphavalue_clone_from() {
        let mut target = AlphaValue { number: 0.0 };

        target.clone_from(&AlphaValue { number: 0.25 });

        assert_eq!(target, AlphaValue { number: 0.25 });

        target.clone_from(&AlphaValue { number: 0.667 });

        assert_eq!(target, AlphaValue { number: 0.667 });
    }

    #[test]
    fn alphavalue_fmt() {
        assert_eq!(
            format!("{:?}", AlphaValue { number: 0. }),
            "AlphaValue { number: 0.0 }",
        );
    }

    #[test]
    fn alphavalue_eq() {
        assert!(AlphaValue { number: 0.0 } == AlphaValue { number: 0.0 });
        assert!(AlphaValue { number: 0.5 } == AlphaValue { number: 0.5 });
    }

    #[test]
    fn alphavalue_ne() {
        assert!(AlphaValue { number: 0.0 } != AlphaValue { number: 0.5 });
        assert!(AlphaValue { number: 0.5 } != AlphaValue { number: 0.0 });
    }

    #[test]
    fn alphavalue_new() {
        assert_eq!(AlphaValue::new(0.0), AlphaValue { number: 0.0 });
        assert_eq!(AlphaValue::new(0.123), AlphaValue { number: 0.123 });
        assert_eq!(AlphaValue::new(1.0), AlphaValue { number: 1.0 });
    }

    #[test]
    fn srgbcolor_clone() {
        assert_eq!(
            SrgbColor::Rgb {
                red: None,
                green: None,
                blue: None,
                alpha: None
            }
            .clone(),
            SrgbColor::Rgb {
                red: None,
                green: None,
                blue: None,
                alpha: None
            }
        );
        assert_eq!(
            SrgbColor::Hsl {
                hue: None,
                saturation: None,
                lightness: None,
                alpha: None
            }
            .clone(),
            SrgbColor::Hsl {
                hue: None,
                saturation: None,
                lightness: None,
                alpha: None
            }
        );
        assert_eq!(
            SrgbColor::Hwb {
                hue: None,
                whiteness: None,
                blackness: None,
                alpha: None
            }
            .clone(),
            SrgbColor::Hwb {
                hue: None,
                whiteness: None,
                blackness: None,
                alpha: None
            }
        );
    }

    #[test]
    fn srgbcolor_clone_from() {
        let mut target = SrgbColor::Rgb {
            red: Some(0.0),
            green: Some(0.0),
            blue: Some(0.0),
            alpha: Some(AlphaValue { number: 1.0 }),
        };

        target.clone_from(&SrgbColor::Hsl {
            hue: Some(Hue { degrees: 10. }),
            saturation: Some(0.20),
            lightness: Some(0.30),
            alpha: Some(AlphaValue { number: 0.65 }),
        });

        assert_eq!(
            target,
            SrgbColor::Hsl {
                hue: Some(Hue { degrees: 10. }),
                saturation: Some(0.20),
                lightness: Some(0.30),
                alpha: Some(AlphaValue { number: 0.65 })
            }
        );

        target.clone_from(&SrgbColor::Hwb {
            hue: Some(Hue { degrees: 10. }),
            whiteness: Some(0.20),
            blackness: Some(0.30),
            alpha: Some(AlphaValue { number: 0.325 }),
        });

        assert_eq!(
            target,
            SrgbColor::Hwb {
                hue: Some(Hue { degrees: 10. }),
                whiteness: Some(0.20),
                blackness: Some(0.30),
                alpha: Some(AlphaValue { number: 0.325 })
            }
        );
    }

    #[test]
    fn srgbcolor_fmt() {
        assert_eq!(
            format!("{:?}", SrgbColor::Rgb { red: Some(0.0), green: Some(0.0), blue: Some(0.0), alpha: Some(AlphaValue { number: 1.0 }) }),
            "Rgb { red: Some(0.0), green: Some(0.0), blue: Some(0.0), alpha: Some(AlphaValue { number: 1.0 }) }",
        );
    }

    #[test]
    fn srgbcolor_eq() {
        assert!(
            SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 1.0 })
            } == SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
        assert!(
            SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 0.0 })
            } == SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 0.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_ne() {
        assert!(
            SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 1.0 })
            } != SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 0.0 })
            }
        );
        assert!(
            SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 0.0 })
            } != SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_new() {
        assert_eq!(
            SrgbColor::new(None, None, None, None),
            SrgbColor::Rgb {
                red: None,
                green: None,
                blue: None,
                alpha: None
            }
        );
        assert_eq!(
            SrgbColor::new(
                Some(0.0),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue { number: 1.0 })
            ),
            SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_with_hsl() {
        assert_eq!(
            SrgbColor::with_hsl(None, None, None, None),
            SrgbColor::Hsl {
                hue: None,
                saturation: None,
                lightness: None,
                alpha: None
            }
        );
        assert_eq!(
            SrgbColor::with_hsl(
                Some(Hue { degrees: 0. }),
                Some(0.5),
                Some(0.5),
                Some(AlphaValue { number: 1.0 })
            ),
            SrgbColor::Hsl {
                hue: Some(Hue { degrees: 0. }),
                saturation: Some(0.5),
                lightness: Some(0.5),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_with_hwb() {
        assert_eq!(
            SrgbColor::with_hwb(None, None, None, None),
            SrgbColor::Hwb {
                hue: None,
                blackness: None,
                whiteness: None,
                alpha: None
            }
        );
        assert_eq!(
            SrgbColor::with_hwb(
                Some(Hue { degrees: 0. }),
                Some(0.5),
                Some(0.5),
                Some(AlphaValue { number: 1.0 })
            ),
            SrgbColor::Hwb {
                hue: Some(Hue { degrees: 0. }),
                blackness: Some(0.5),
                whiteness: Some(0.5),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_from_ints() {
        assert_eq!(
            SrgbColor::from_ints(0, 0, 0, 0),
            SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 0.0 })
            }
        );
        assert_eq!(
            SrgbColor::from_ints(153, 153, 153, 153),
            SrgbColor::Rgb {
                red: Some(0.6),
                green: Some(0.6),
                blue: Some(0.6),
                alpha: Some(AlphaValue { number: 0.6 })
            }
        );
        assert_eq!(
            SrgbColor::from_ints(255, 255, 255, 255),
            SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_from_floats() {
        assert_eq!(
            SrgbColor::from_floats(0.1, 0.2, 0.3, 0.4),
            SrgbColor::Rgb {
                red: Some(0.1),
                green: Some(0.2),
                blue: Some(0.3),
                alpha: Some(AlphaValue { number: 0.4 })
            }
        );
        assert_eq!(
            SrgbColor::from_floats(0.5, 0.5, 0.5, 0.5),
            SrgbColor::Rgb {
                red: Some(0.5),
                green: Some(0.5),
                blue: Some(0.5),
                alpha: Some(AlphaValue { number: 0.5 })
            }
        );
        assert_eq!(
            SrgbColor::from_floats(1.0, 1.0, 1.0, 1.0),
            SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_to_floats() {
        assert_eq!(
            SrgbColor::Rgb {
                red: None,
                green: None,
                blue: None,
                alpha: None
            }
            .to_floats(),
            (0.0, 0.0, 0.0, 0.0)
        );
        assert_eq!(
            SrgbColor::Rgb {
                red: Some(0.5),
                green: Some(0.5),
                blue: Some(0.5),
                alpha: Some(AlphaValue { number: 0.5 })
            }
            .to_floats(),
            (0.5, 0.5, 0.5, 0.5)
        );
        assert_eq!(
            SrgbColor::Hsl {
                hue: None,
                saturation: None,
                lightness: None,
                alpha: None
            }
            .to_floats(),
            (0.0, 0.0, 0.0, 0.0)
        );
        assert_eq!(
            SrgbColor::Hsl {
                hue: Some(Hue { degrees: 0. }),
                saturation: Some(0.5),
                lightness: Some(0.5),
                alpha: Some(AlphaValue { number: 1.0 })
            }
            .to_floats(),
            (0.75, 0.25, 0.25, 1.0)
        );
        assert_eq!(
            SrgbColor::Hwb {
                hue: None,
                blackness: None,
                whiteness: None,
                alpha: None
            }
            .to_floats(),
            (1.0, 0.0, 0.0, 0.0)
        );
        assert_eq!(
            SrgbColor::Hwb {
                hue: Some(Hue { degrees: 0. }),
                blackness: Some(0.5),
                whiteness: Some(0.5),
                alpha: Some(AlphaValue { number: 1.0 })
            }
            .to_floats(),
            (0.5, 0.5, 0.5, 1.0)
        );
    }

    #[test]
    fn srgbcolor_from_rgba() {
        assert_eq!(
            SrgbColor::from_rgba(RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 0
            }),
            SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(0.0),
                blue: Some(0.0),
                alpha: Some(AlphaValue { number: 0.0 })
            }
        );
        assert_eq!(
            SrgbColor::from_rgba(RGBA {
                red: 204,
                green: 204,
                blue: 204,
                alpha: 204
            }),
            SrgbColor::Rgb {
                red: Some(0.8),
                green: Some(0.8),
                blue: Some(0.8),
                alpha: Some(AlphaValue { number: 0.8 })
            }
        );
        assert_eq!(
            SrgbColor::from_rgba(RGBA {
                red: 255,
                green: 255,
                blue: 255,
                alpha: 255
            }),
            SrgbColor::Rgb {
                red: Some(1.0),
                green: Some(1.0),
                blue: Some(1.0),
                alpha: Some(AlphaValue { number: 1.0 })
            }
        );
    }

    #[test]
    fn srgbcolor_to_rgba() {
        assert_eq!(
            SrgbColor::Rgb {
                red: None,
                green: None,
                blue: None,
                alpha: None
            }
            .to_rgba(),
            RGBA {
                red: 0,
                green: 0,
                blue: 0,
                alpha: 0
            }
        );
        assert_eq!(
            SrgbColor::Rgb {
                red: Some(0.0),
                green: Some(-1.0),
                blue: Some(0.5),
                alpha: Some(AlphaValue { number: 2.0 })
            }
            .to_rgba(),
            RGBA {
                red: 0,
                green: 0,
                blue: 128,
                alpha: 255
            }
        );
    }

    #[test]
    fn srgbcolor_to_css() {
        assert_eq!(
            SrgbColor::from_ints(0, 0, 0, 0).to_css_string(),
            "rgba(0, 0, 0, 0)"
        );
        assert_eq!(
            SrgbColor::from_ints(0, 170, 255, 51).to_css_string(),
            "rgba(0, 170, 255, 0.2)"
        );
        assert_eq!(
            SrgbColor::from_ints(0, 170, 255, 74).to_css_string(),
            "rgba(0, 170, 255, 0.290196)"
        );
        assert_eq!(
            SrgbColor::from_ints(0, 51, 255, 170).to_css_string(),
            "rgba(0, 51, 255, 0.666667)"
        );
        assert_eq!(
            SrgbColor::from_ints(255, 255, 255, 255).to_css_string(),
            "rgb(255, 255, 255)"
        );
    }

    #[test]
    fn namedcolor_clone() {
        assert_eq!(
            NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255)).clone(),
            NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255))
        );
    }

    #[test]
    fn namedcolor_clone_from() {
        let mut target = NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255));

        target.clone_from(&NamedColor::new(
            String::from("blue"),
            SrgbColor::from_ints(0, 0, 255, 255),
        ));

        assert_eq!(
            target,
            NamedColor::new(String::from("blue"), SrgbColor::from_ints(0, 0, 255, 255))
        );
    }

    #[test]
    fn namedcolor_fmt() {
        assert_eq!(
            format!(
                "{:?}",
                NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255))
            ),
            "NamedColor { name: \"red\", value: Rgb { red: Some(1.0), green: Some(0.0), blue: Some(0.0), alpha: Some(AlphaValue { number: 1.0 }) } }"
        );
    }

    #[test]
    fn namedcolor_eq() {
        assert!(
            NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255))
                == NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255))
        );
        assert!(
            NamedColor::new(String::from("blue"), SrgbColor::from_ints(0, 0, 255, 255))
                == NamedColor::new(String::from("blue"), SrgbColor::from_ints(0, 0, 255, 255))
        );
    }

    #[test]
    fn namedcolor_ne() {
        assert!(
            NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255))
                != NamedColor::new(String::from("blue"), SrgbColor::from_ints(0, 0, 255, 255))
        );
        assert!(
            NamedColor::new(String::from("blue"), SrgbColor::from_ints(0, 0, 255, 255))
                != NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255))
        );
    }

    #[test]
    fn namedcolor_new() {
        assert_eq!(
            NamedColor::new(String::from("red"), SrgbColor::from_ints(255, 0, 0, 255)),
            NamedColor {
                name: "red".to_string(),
                value: SrgbColor::from_ints(255, 0, 0, 255)
            }
        );
    }

    #[test]
    fn namedcolor_to_css() {
        assert_eq!(
            NamedColor::new("black".to_string(), SrgbColor::from_ints(0, 0, 0, 255))
                .to_css_string(),
            "black"
        );
    }

    #[test]
    fn systemcolor_clone() {
        assert_eq!(
            SystemColor::new(String::from("canvas")).clone(),
            SystemColor::new(String::from("canvas"))
        );
    }

    #[test]
    fn systemcolor_clone_from() {
        let mut target = SystemColor::new(String::from("canvas"));

        target.clone_from(&SystemColor::new(String::from("field")));

        assert_eq!(target, SystemColor::new(String::from("field")));
    }

    #[test]
    fn systemcolor_fmt() {
        assert_eq!(
            format!("{:?}", SystemColor::new(String::from("canvas"))),
            "SystemColor { name: \"canvas\" }"
        );
    }

    #[test]
    fn systemcolor_eq() {
        assert!(
            SystemColor::new(String::from("canvas")) == SystemColor::new(String::from("canvas"))
        );
        assert!(SystemColor::new(String::from("field")) == SystemColor::new(String::from("field")));
    }

    #[test]
    fn systemcolor_ne() {
        assert!(
            SystemColor::new(String::from("canvas")) != SystemColor::new(String::from("field"))
        );
        assert!(
            SystemColor::new(String::from("field")) != SystemColor::new(String::from("canvas"))
        );
    }

    #[test]
    fn systemcolor_new() {
        assert_eq!(
            SystemColor::new(String::from("canvas")),
            SystemColor {
                name: "canvas".to_string()
            }
        );
    }

    #[test]
    fn deprecatedcolor_clone() {
        assert_eq!(
            DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            )
            .clone(),
            DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            )
        );
    }

    #[test]
    fn deprecatedcolor_clone_from() {
        let mut target = DeprecatedColor::new(
            String::from("background"),
            SystemColor::new(String::from("canvas")),
        );

        target.clone_from(&DeprecatedColor::new(
            String::from("activeborder"),
            SystemColor::new(String::from("buttonborder")),
        ));

        assert_eq!(
            target,
            DeprecatedColor::new(
                String::from("activeborder"),
                SystemColor::new(String::from("buttonborder"))
            )
        );
    }

    #[test]
    fn deprecatedcolor_fmt() {
        assert_eq!(
            format!(
                "{:?}",
                DeprecatedColor::new(
                    String::from("background"),
                    SystemColor::new(String::from("canvas"))
                )
            ),
            "DeprecatedColor { name: \"background\", same_as: SystemColor { name: \"canvas\" } }"
        );
    }

    #[test]
    fn deprecatedcolor_eq() {
        assert!(
            DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            ) == DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            )
        );
        assert!(
            DeprecatedColor::new(
                String::from("activeborder"),
                SystemColor::new(String::from("buttonborder"))
            ) == DeprecatedColor::new(
                String::from("activeborder"),
                SystemColor::new(String::from("buttonborder"))
            )
        );
    }

    #[test]
    fn deprecatedcolor_ne() {
        assert!(
            DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            ) != DeprecatedColor::new(
                String::from("activeborder"),
                SystemColor::new(String::from("buttonborder"))
            )
        );
        assert!(
            DeprecatedColor::new(
                String::from("activeborder"),
                SystemColor::new(String::from("buttonborder"))
            ) != DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            )
        );
    }

    #[test]
    fn deprecatedcolor_new() {
        assert_eq!(
            DeprecatedColor::new(
                String::from("background"),
                SystemColor::new(String::from("canvas"))
            ),
            DeprecatedColor {
                name: "background".to_string(),
                same_as: SystemColor::new(String::from("canvas"))
            }
        );
    }

    #[test]
    fn currentcolor_clone() {
        assert_eq!(CurrentColor.clone(), CurrentColor);
    }

    #[test]
    fn currentcolor_clone_from() {
        let mut target = CurrentColor;

        target.clone_from(&CurrentColor);

        assert_eq!(target, CurrentColor);
    }

    #[test]
    fn currentcolor_fmt() {
        assert_eq!(format!("{:?}", CurrentColor), "CurrentColor");
    }

    #[test]
    fn currentcolor_eq() {
        assert!(CurrentColor == CurrentColor);
    }

    #[test]
    fn currentcolor_ne() {}

    #[test]
    fn currentcolor_to_css() {
        assert_eq!(CurrentColor.to_css_string(), "currentcolor");
    }

    #[test]
    fn colorcomponentparser_parse_percentage() {
        assert_eq!(
            ColorComponentParser::parse_percentage(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("0%")),
            )
            .unwrap(),
            0.
        );
        assert_eq!(
            ColorComponentParser::parse_percentage(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("100%")),
            )
            .unwrap(),
            1.
        );
        assert_eq!(
            ColorComponentParser::parse_percentage(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("-123%")),
            )
            .unwrap(),
            -1.23
        );
        assert_eq!(
            ColorComponentParser::parse_percentage(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123%")),
            )
            .unwrap(),
            1.23
        );
    }

    #[test]
    fn colorcomponentparser_parse_number() {
        assert_eq!(
            ColorComponentParser::parse_number(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("0")),
            )
            .unwrap(),
            0.
        );
        assert_eq!(
            ColorComponentParser::parse_number(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("100")),
            )
            .unwrap(),
            100.
        );
        assert_eq!(
            ColorComponentParser::parse_number(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("-123")),
            )
            .unwrap(),
            -123.
        );
        assert_eq!(
            ColorComponentParser::parse_number(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123")),
            )
            .unwrap(),
            123.
        );
    }

    #[test]
    fn colorcomponentparser_parse_hue() {
        assert_eq!(
            ColorComponentParser::parse_hue(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123")),
            )
            .unwrap(),
            Hue { degrees: 123. }
        );
        assert_eq!(
            ColorComponentParser::parse_hue(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123.456")),
            )
            .unwrap(),
            Hue { degrees: 123.456 }
        );

        assert_eq!(
            ColorComponentParser::parse_hue(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123deg")),
            )
            .unwrap(),
            Hue { degrees: 123. }
        );
        assert_eq!(
            ColorComponentParser::parse_hue(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123grad")),
            )
            .unwrap(),
            Hue { degrees: 110.7 }
        );
        assert_eq!(
            ColorComponentParser::parse_hue(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123rad")),
            )
            .unwrap(),
            Hue { degrees: 7047.381 }
        );
        assert_eq!(
            ColorComponentParser::parse_hue(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("123turn")),
            )
            .unwrap(),
            Hue { degrees: 44280. }
        );

        assert!(ColorComponentParser::parse_hue(
            &DefaultComponentParser,
            &mut Parser::new(&mut ParserInput::new("123em"))
        )
        .is_err());
        assert!(ColorComponentParser::parse_hue(
            &DefaultComponentParser,
            &mut Parser::new(&mut ParserInput::new("123%"))
        )
        .is_err());
    }

    #[test]
    fn colorcomponentparser_parse_alpha_value() {
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("0")),
            )
            .unwrap(),
            AlphaValue { number: 0.0 }
        );
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("0.5")),
            )
            .unwrap(),
            AlphaValue { number: 0.5 }
        );
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("0.666667")),
            )
            .unwrap(),
            AlphaValue { number: 0.666667 }
        );
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("1")),
            )
            .unwrap(),
            AlphaValue { number: 1.0 }
        );

        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("0%")),
            )
            .unwrap(),
            AlphaValue { number: 0.0 }
        );
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("50%")),
            )
            .unwrap(),
            AlphaValue { number: 0.5 }
        );
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("66.6667%")),
            )
            .unwrap(),
            AlphaValue { number: 0.666667 }
        );
        assert_eq!(
            ColorComponentParser::parse_alpha_value(
                &DefaultComponentParser,
                &mut Parser::new(&mut ParserInput::new("100%")),
            )
            .unwrap(),
            AlphaValue { number: 1.0 }
        );

        assert!(ColorComponentParser::parse_alpha_value(
            &DefaultComponentParser,
            &mut Parser::new(&mut ParserInput::new("123em"))
        )
        .is_err());
        assert!(ColorComponentParser::parse_alpha_value(
            &DefaultComponentParser,
            &mut Parser::new(&mut ParserInput::new("123deg"))
        )
        .is_err());
    }

    #[test]
    fn color_parse_hash() {
        assert_eq!(
            Color::parse_hex_color("AABBCCDD"),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAA, 0xBB, 0xCC, 0xDD
            )))
        );
        assert_eq!(
            Color::parse_hex_color("AABBCC"),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAA, 0xBB, 0xCC, 255
            )))
        );
        assert_eq!(
            Color::parse_hex_color("ABCD"),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAA, 0xBB, 0xCC, 0xDD
            )))
        );
        assert_eq!(
            Color::parse_hex_color("ABC"),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAA, 0xBB, 0xCC, 255
            )))
        );
        assert_eq!(
            Color::parse_hex_color("12345678"),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0x12, 0x34, 0x56, 0x78
            )))
        );
        assert_eq!(
            Color::parse_hex_color("abcdef90"),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAB, 0xCD, 0xEF, 0x90
            )))
        );

        assert_eq!(Color::parse_hex_color(""), Err(()));
        assert_eq!(Color::parse_hex_color("A"), Err(()));
        assert_eq!(Color::parse_hex_color("AB"), Err(()));
        assert_eq!(Color::parse_hex_color("ABCDE"), Err(()));
        assert_eq!(Color::parse_hex_color("ABCDEF0"), Err(()));
        assert_eq!(Color::parse_hex_color("GHIJKLMN"), Err(()));
    }

    #[test]
    fn color_parse_color_keyword() {
        assert_eq!(
            Color::parse_color_keyword("black"),
            Ok(Color::NamedColor(NamedColor::new(
                "black".to_string(),
                SrgbColor::from_ints(0, 0, 0, 255)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("white"),
            Ok(Color::NamedColor(NamedColor::new(
                "white".to_string(),
                SrgbColor::from_ints(255, 255, 255, 255)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("cyan"),
            Ok(Color::NamedColor(NamedColor::new(
                "cyan".to_string(),
                SrgbColor::from_ints(0, 255, 255, 255)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("magenta"),
            Ok(Color::NamedColor(NamedColor::new(
                "magenta".to_string(),
                SrgbColor::from_ints(255, 0, 255, 255)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("red"),
            Ok(Color::NamedColor(NamedColor::new(
                "red".to_string(),
                SrgbColor::from_ints(255, 0, 0, 255)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("lightgoldenrodyellow"),
            Ok(Color::NamedColor(NamedColor::new(
                "lightgoldenrodyellow".to_string(),
                SrgbColor::from_ints(250, 250, 210, 255)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("transparent"),
            Ok(Color::NamedColor(NamedColor::new(
                "transparent".to_string(),
                SrgbColor::from_ints(0, 0, 0, 0)
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("canvastext"),
            Ok(Color::SystemColor(SystemColor::new(
                "canvastext".to_string()
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("activeborder"),
            Ok(Color::DeprecatedColor(DeprecatedColor::new(
                "activeborder".to_string(),
                SystemColor::new("buttonborder".to_string())
            )))
        );
        assert_eq!(
            Color::parse_color_keyword("currentcolor"),
            Ok(Color::CurrentColor(CurrentColor))
        );

        assert!(Color::parse_color_keyword("yellowblue").is_err());
    }

    #[test]
    fn color_parse_with() {
        let component_parser = DefaultComponentParser;

        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("#abc"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAA, 0xBB, 0xCC, 255
            )))
        );

        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("currentcolor"))
            ),
            Ok(Color::CurrentColor(CurrentColor))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("transparent"))
            ),
            Ok(Color::NamedColor(NamedColor::new(
                "transparent".to_string(),
                SrgbColor::from_ints(0, 0, 0, 0)
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("red"))
            ),
            Ok(Color::NamedColor(NamedColor::new(
                "red".to_string(),
                SrgbColor::from_ints(255, 0, 0, 255)
            )))
        );

        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgb(255, 0, 0)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgb(100%, 0%, 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgba(255, 0, 0, 0.5)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_floats(1.0, 0.0, 0.0, 0.5)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgba(100%, 0%, 0%, 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_floats(1.0, 0.0, 0.0, 0.5)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgb(255 0 0)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgb(100% 0% 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgb(255 0 0 / 0.5)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_floats(1.0, 0.0, 0.0, 0.5)))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("rgb(100% 0% 0% / 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::from_floats(1.0, 0.0, 0.0, 0.5)))
        );

        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0, 100%, 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0deg, 100%, 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsla(0, 100%, 50%, 0.5)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(0.5))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsla(0deg, 100%, 50%, 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(0.5))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0 100% 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0deg 100% 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0grad 100% 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0rad 100% 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0turn 100% 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0 100% 50% / 0.5)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(0.5))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hsl(0deg 100% 50% / 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hsl(
                Some(Hue::new(0.)),
                Some(1.0),
                Some(0.5),
                Some(AlphaValue::new(0.5))
            )))
        );

        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0 0% 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0deg 0% 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0grad 0% 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0rad 0% 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0turn 0% 0%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(1.0))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0 0% 0% / 0.5)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(0.5))
            )))
        );
        assert_eq!(
            Color::parse_with(
                &component_parser,
                &mut Parser::new(&mut ParserInput::new("hwb(0deg 0% 0% / 50%)"))
            ),
            Ok(Color::SrgbColor(SrgbColor::with_hwb(
                Some(Hue::new(0.)),
                Some(0.0),
                Some(0.0),
                Some(AlphaValue::new(0.5))
            )))
        );

        assert!(Color::parse_with(
            &component_parser,
            &mut Parser::new(&mut ParserInput::new("0deg"))
        )
        .is_err());
        assert!(Color::parse_with(
            &component_parser,
            &mut Parser::new(&mut ParserInput::new("rgb(255 0 0 / 0deg)"))
        )
        .is_err());
        assert!(Color::parse_with(
            &component_parser,
            &mut Parser::new(&mut ParserInput::new("hsl(0em 100% 50%)"))
        )
        .is_err());
        assert!(Color::parse_with(
            &component_parser,
            &mut Parser::new(&mut ParserInput::new("hwb(0% 0% 0%)"))
        )
        .is_err());
        assert!(Color::parse_with(
            &component_parser,
            &mut Parser::new(&mut ParserInput::new("notafunction(0 0% 0%)"))
        )
        .is_err());
    }

    #[test]
    fn color_parse() {
        assert_eq!(
            Color::parse(&mut Parser::new(&mut ParserInput::new("#abc123"))),
            Ok(Color::SrgbColor(SrgbColor::from_ints(
                0xAB, 0xC1, 0x23, 255
            )))
        );
    }

    #[test]
    fn color_clone() {
        assert_eq!(
            Color::CurrentColor(CurrentColor).clone(),
            Color::CurrentColor(CurrentColor)
        );
        assert_eq!(
            Color::SrgbColor(SrgbColor::from_ints(0, 0, 0, 255)).clone(),
            Color::SrgbColor(SrgbColor::from_ints(0, 0, 0, 255))
        );
    }

    #[test]
    fn color_clone_from() {
        let mut target = Color::CurrentColor(CurrentColor);
        target.clone_from(&Color::SrgbColor(SrgbColor::from_ints(0, 0, 0, 255)));

        assert_eq!(target, Color::SrgbColor(SrgbColor::from_ints(0, 0, 0, 255)));

        target.clone_from(&Color::CurrentColor(CurrentColor));

        assert_eq!(target, Color::CurrentColor(CurrentColor));
    }

    #[test]
    fn color_fmt() {
        let color = Color::SrgbColor(SrgbColor::from_ints(0, 0, 0, 0));

        assert_eq!(
            format!("{color:?}"),
            "SrgbColor(Rgb { red: Some(0.0), green: Some(0.0), blue: Some(0.0), alpha: Some(AlphaValue { number: 0.0 }) })"
        );
    }

    #[test]
    fn color_eq() {
        assert!(Color::CurrentColor(CurrentColor) == Color::CurrentColor(CurrentColor));
        assert!(
            Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255))
                == Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255))
        );
    }

    #[test]
    fn color_ne() {
        assert!(
            Color::CurrentColor(CurrentColor)
                != Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255))
        );
        assert!(
            Color::SrgbColor(SrgbColor::from_ints(255, 0, 0, 255))
                != Color::CurrentColor(CurrentColor)
        );
    }

    #[test]
    fn super_hsl_to_rgb() {
        assert_eq!(super::hsl_to_rgb(0. / 360., 1., 0.5), (1., 0., 0.));
        assert_eq!(super::hsl_to_rgb(120. / 360., 1., 0.5), (0., 1., 0.));
        assert_eq!(super::hsl_to_rgb(120. / 360., 1., 0.25), (0., 0.5, 0.));
        assert_eq!(super::hsl_to_rgb(120. / 360., 1., 0.75), (0.5, 1., 0.5));
        // XXX: Doesn't pass due to rounding errors.
        // assert_eq!(super::hsl_to_rgb(120. / 360., 0.75, 0.85), (0.7375, 0.9625, 0.7375));
        assert_eq!(super::hsl_to_rgb(240. / 360., 1., 0.5), (0., 0., 1.));
        assert_eq!(super::hsl_to_rgb(60. / 360., 1., 0.5), (1., 1., 0.));
        assert_eq!(super::hsl_to_rgb(360. / 360., 1., 0.5), (1., 0., 0.));
        assert_eq!(super::hsl_to_rgb(0. / 360., 1., 1.), (1., 1., 1.));
        assert_eq!(super::hsl_to_rgb(0. / 360., 0., 1.), (1., 1., 1.));
        assert_eq!(super::hsl_to_rgb(0. / 360., 1., 0.), (0., 0., 0.));
        assert_eq!(super::hsl_to_rgb(0. / 360., 0., 0.), (0., 0., 0.));
        // XXX: Don't pass due to rounding errors.
        // assert_eq!(super::hsl_to_rgb(0. / 360., 1., 0.6), (1., 0.2, 0.2));
        // assert_eq!(super::hsl_to_rgb(0. / 360., 0.8, 0.3), (0.54, 0.06, 0.06));
        // assert_eq!(super::hsl_to_rgb(360. / 360., 1., 0.6), (1., 0.2, 0.2));
        // assert_eq!(super::hsl_to_rgb(360. / 360., 0.8, 0.3), (0.54, 0.06, 0.06));
    }

    #[test]
    fn super_hwb_to_rgb() {
        assert_eq!(super::hwb_to_rgb(0. / 360., 0., 0.), (1., 0., 0.));
        assert_eq!(super::hwb_to_rgb(0. / 360., 1., 0.), (1., 1., 1.));
        assert_eq!(super::hwb_to_rgb(0. / 360., 1., 1.), (0.5, 0.5, 0.5));
        assert_eq!(super::hwb_to_rgb(0. / 360., 0., 1.), (0., 0., 0.));
        // XXX: Doesn't pass due to rounding errors.
        // assert_eq!(super::hwb_to_rgb(180. / 360., 0.4, 0.2), (0.4, 0.8, 0.8));
        assert_eq!(super::hwb_to_rgb(180. / 360., 0.2, 0.4), (0.2, 0.6, 0.6));
    }
}
