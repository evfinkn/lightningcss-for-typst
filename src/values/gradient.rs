//! CSS gradient values.

use super::angle::{Angle, AnglePercentage};
use super::color::{ColorFallbackKind, CssColor};
use super::length::{Length, LengthPercentage};
use super::number::CSSNumber;
use super::percentage::{DimensionPercentage, NumberOrPercentage, Percentage};
use super::position::{HorizontalPositionKeyword, VerticalPositionKeyword};
use super::position::{Position, PositionComponent};
use crate::compat;
use crate::error::{ParserError, PrinterError};
use crate::macros::enum_property;
use crate::prefixes::Feature;
use crate::printer::Printer;
use crate::targets::{Browsers, Targets};
use crate::traits::private::TryAdd;
use crate::traits::{IsCompatible, Parse, ToTypst, TrySign, Zero};
use crate::vendor_prefix::VendorPrefix;
#[cfg(feature = "visitor")]
use crate::visitor::Visit;
use cssparser::*;

#[cfg(feature = "serde")]
use crate::serialization::ValueWrapper;

/// A CSS [`<gradient>`](https://www.w3.org/TR/css-images-3/#gradients) value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum Gradient {
  /// A `linear-gradient()`, and its vendor prefix.
  Linear(LinearGradient),
  /// A `repeating-linear-gradient()`, and its vendor prefix.
  RepeatingLinear(LinearGradient),
  /// A `radial-gradient()`, and its vendor prefix.
  Radial(RadialGradient),
  /// A `repeating-radial-gradient`, and its vendor prefix.
  RepeatingRadial(RadialGradient),
  /// A `conic-gradient()`.
  Conic(ConicGradient),
  /// A `repeating-conic-gradient()`.
  RepeatingConic(ConicGradient),
  /// A legacy `-webkit-gradient()`.
  #[cfg_attr(feature = "serde", serde(rename = "webkit-gradient"))]
  WebKitGradient(WebKitGradient),
}

impl Gradient {
  /// Returns the vendor prefix of the gradient.
  pub fn get_vendor_prefix(&self) -> VendorPrefix {
    match self {
      Gradient::Linear(LinearGradient { vendor_prefix, .. })
      | Gradient::RepeatingLinear(LinearGradient { vendor_prefix, .. })
      | Gradient::Radial(RadialGradient { vendor_prefix, .. })
      | Gradient::RepeatingRadial(RadialGradient { vendor_prefix, .. }) => *vendor_prefix,
      Gradient::WebKitGradient(_) => VendorPrefix::WebKit,
      _ => VendorPrefix::None,
    }
  }

  /// Returns the vendor prefixes needed for the given browser targets.
  pub fn get_necessary_prefixes(&self, targets: Targets) -> VendorPrefix {
    macro_rules! get_prefixes {
      ($feature: ident, $prefix: expr) => {
        targets.prefixes($prefix, Feature::$feature)
      };
    }

    match self {
      Gradient::Linear(linear) => get_prefixes!(LinearGradient, linear.vendor_prefix),
      Gradient::RepeatingLinear(linear) => get_prefixes!(RepeatingLinearGradient, linear.vendor_prefix),
      Gradient::Radial(radial) => get_prefixes!(RadialGradient, radial.vendor_prefix),
      Gradient::RepeatingRadial(radial) => get_prefixes!(RepeatingRadialGradient, radial.vendor_prefix),
      _ => VendorPrefix::None,
    }
  }

  /// Returns a copy of the gradient with the given vendor prefix.
  pub fn get_prefixed(&self, prefix: VendorPrefix) -> Gradient {
    match self {
      Gradient::Linear(linear) => Gradient::Linear(LinearGradient {
        vendor_prefix: prefix,
        ..linear.clone()
      }),
      Gradient::RepeatingLinear(linear) => Gradient::RepeatingLinear(LinearGradient {
        vendor_prefix: prefix,
        ..linear.clone()
      }),
      Gradient::Radial(radial) => Gradient::Radial(RadialGradient {
        vendor_prefix: prefix,
        ..radial.clone()
      }),
      Gradient::RepeatingRadial(radial) => Gradient::RepeatingRadial(RadialGradient {
        vendor_prefix: prefix,
        ..radial.clone()
      }),
      _ => self.clone(),
    }
  }

  /// Attempts to convert the gradient to the legacy `-webkit-gradient()` syntax.
  ///
  /// Returns an error in case the conversion is not possible.
  pub fn get_legacy_webkit(&self) -> Result<Gradient, ()> {
    Ok(Gradient::WebKitGradient(WebKitGradient::from_standard(self)?))
  }

  /// Returns the color fallback types needed for the given browser targets.
  pub fn get_necessary_fallbacks(&self, targets: Targets) -> ColorFallbackKind {
    match self {
      Gradient::Linear(LinearGradient { items, .. })
      | Gradient::Radial(RadialGradient { items, .. })
      | Gradient::RepeatingLinear(LinearGradient { items, .. })
      | Gradient::RepeatingRadial(RadialGradient { items, .. }) => {
        let mut fallbacks = ColorFallbackKind::empty();
        for item in items {
          fallbacks |= item.get_necessary_fallbacks(targets)
        }
        fallbacks
      }
      Gradient::Conic(ConicGradient { items, .. }) | Gradient::RepeatingConic(ConicGradient { items, .. }) => {
        let mut fallbacks = ColorFallbackKind::empty();
        for item in items {
          fallbacks |= item.get_necessary_fallbacks(targets)
        }
        fallbacks
      }
      Gradient::WebKitGradient(..) => ColorFallbackKind::empty(),
    }
  }

  /// Returns a fallback gradient for the given color fallback type.
  pub fn get_fallback(&self, kind: ColorFallbackKind) -> Gradient {
    match self {
      Gradient::Linear(g) => Gradient::Linear(g.get_fallback(kind)),
      Gradient::RepeatingLinear(g) => Gradient::RepeatingLinear(g.get_fallback(kind)),
      Gradient::Radial(g) => Gradient::Radial(g.get_fallback(kind)),
      Gradient::RepeatingRadial(g) => Gradient::RepeatingRadial(g.get_fallback(kind)),
      Gradient::Conic(g) => Gradient::Conic(g.get_fallback(kind)),
      Gradient::RepeatingConic(g) => Gradient::RepeatingConic(g.get_fallback(kind)),
      Gradient::WebKitGradient(g) => Gradient::WebKitGradient(g.get_fallback(kind)),
    }
  }
}

impl<'i> Parse<'i> for Gradient {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let func = input.expect_function()?.clone();
    input.parse_nested_block(|input| {
      match_ignore_ascii_case! { &func,
        "linear-gradient" => Ok(Gradient::Linear(LinearGradient::parse(input, VendorPrefix::None)?)),
        "repeating-linear-gradient" => Ok(Gradient::RepeatingLinear(LinearGradient::parse(input, VendorPrefix::None)?)),
        "radial-gradient" => Ok(Gradient::Radial(RadialGradient::parse(input, VendorPrefix::None)?)),
        "repeating-radial-gradient" => Ok(Gradient::RepeatingRadial(RadialGradient::parse(input, VendorPrefix::None)?)),
        "conic-gradient" => Ok(Gradient::Conic(ConicGradient::parse(input)?)),
        "repeating-conic-gradient" => Ok(Gradient::RepeatingConic(ConicGradient::parse(input)?)),
        "-webkit-linear-gradient" => Ok(Gradient::Linear(LinearGradient::parse(input, VendorPrefix::WebKit)?)),
        "-webkit-repeating-linear-gradient" => Ok(Gradient::RepeatingLinear(LinearGradient::parse(input, VendorPrefix::WebKit)?)),
        "-webkit-radial-gradient" => Ok(Gradient::Radial(RadialGradient::parse(input, VendorPrefix::WebKit)?)),
        "-webkit-repeating-radial-gradient" => Ok(Gradient::RepeatingRadial(RadialGradient::parse(input, VendorPrefix::WebKit)?)),
        "-moz-linear-gradient" => Ok(Gradient::Linear(LinearGradient::parse(input, VendorPrefix::Moz)?)),
        "-moz-repeating-linear-gradient" => Ok(Gradient::RepeatingLinear(LinearGradient::parse(input, VendorPrefix::Moz)?)),
        "-moz-radial-gradient" => Ok(Gradient::Radial(RadialGradient::parse(input, VendorPrefix::Moz)?)),
        "-moz-repeating-radial-gradient" => Ok(Gradient::RepeatingRadial(RadialGradient::parse(input, VendorPrefix::Moz)?)),
        "-o-linear-gradient" => Ok(Gradient::Linear(LinearGradient::parse(input, VendorPrefix::O)?)),
        "-o-repeating-linear-gradient" => Ok(Gradient::RepeatingLinear(LinearGradient::parse(input, VendorPrefix::O)?)),
        "-o-radial-gradient" => Ok(Gradient::Radial(RadialGradient::parse(input, VendorPrefix::O)?)),
        "-o-repeating-radial-gradient" => Ok(Gradient::RepeatingRadial(RadialGradient::parse(input, VendorPrefix::O)?)),
        "-webkit-gradient" => Ok(Gradient::WebKitGradient(WebKitGradient::parse(input)?)),
        _ => Err(location.new_unexpected_token_error(cssparser::Token::Ident(func.clone())))
      }
    })
  }
}

impl ToTypst for Gradient {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    let (f, prefix, repeating) = match self {
      Gradient::Linear(g) => ("linear-gradient(", Some(g.vendor_prefix), false),
      Gradient::RepeatingLinear(g) => ("repeating-linear-gradient(", Some(g.vendor_prefix), true),
      Gradient::Radial(g) => ("radial-gradient(", Some(g.vendor_prefix), false),
      Gradient::RepeatingRadial(g) => ("repeating-radial-gradient(", Some(g.vendor_prefix), true),
      Gradient::Conic(_) => ("conic-gradient(", None, false),
      Gradient::RepeatingConic(_) => ("repeating-conic-gradient(", None, true),
      Gradient::WebKitGradient(_) => ("-webkit-gradient(", None, false),
    };

    if let Some(prefix) = prefix {
      prefix.to_typst(dest)?;
    }

    dest.write_str(f)?;

    match self {
      Gradient::Linear(linear) | Gradient::RepeatingLinear(linear) => linear.to_typst(dest, repeating)?,
      Gradient::Radial(radial) | Gradient::RepeatingRadial(radial) => radial.to_typst(dest, repeating)?,
      Gradient::Conic(conic) | Gradient::RepeatingConic(conic) => conic.to_typst(dest, repeating)?,
      Gradient::WebKitGradient(g) => g.to_typst(dest)?,
    }

    dest.write_char(')')
  }
}

/// A CSS [`linear-gradient()`](https://www.w3.org/TR/css-images-3/#linear-gradients) or `repeating-linear-gradient()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct LinearGradient {
  /// The vendor prefixes for the gradient.
  pub vendor_prefix: VendorPrefix,
  /// The direction of the gradient.
  pub direction: LineDirection,
  /// The color stops and transition hints for the gradient.
  pub items: Vec<GradientItem<LengthPercentage>>,
}

impl LinearGradient {
  fn parse<'i, 't>(
    input: &mut Parser<'i, 't>,
    vendor_prefix: VendorPrefix,
  ) -> Result<LinearGradient, ParseError<'i, ParserError<'i>>> {
    let direction = if let Ok(direction) =
      input.try_parse(|input| LineDirection::parse(input, vendor_prefix != VendorPrefix::None))
    {
      input.expect_comma()?;
      direction
    } else {
      LineDirection::Vertical(VerticalPositionKeyword::Bottom)
    };
    let items = parse_items(input)?;
    Ok(LinearGradient {
      direction,
      items,
      vendor_prefix,
    })
  }

  fn get_fallback(&self, kind: ColorFallbackKind) -> LinearGradient {
    LinearGradient {
      direction: self.direction.clone(),
      items: self.items.iter().map(|item| item.get_fallback(kind)).collect(),
      vendor_prefix: self.vendor_prefix,
    }
  }
}

impl LinearGradient {
  fn to_typst<W>(&self, dest: &mut Printer<W>, repeating: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    serialize_items(&self.items, repeating, dest)?;
    dest.delim(',', false)?;
    self.direction.to_typst(dest)
  }
}

impl IsCompatible for LinearGradient {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.items.iter().all(|item| item.is_compatible(browsers))
  }
}

/// A CSS [`radial-gradient()`](https://www.w3.org/TR/css-images-3/#radial-gradients) or `repeating-radial-gradient()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(rename_all = "camelCase")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct RadialGradient {
  /// The vendor prefixes for the gradient.
  pub vendor_prefix: VendorPrefix,
  /// The shape of the gradient.
  pub shape: EndingShape,
  /// The position of the gradient.
  pub position: Position,
  /// The color stops and transition hints for the gradient.
  pub items: Vec<GradientItem<LengthPercentage>>,
}

impl<'i> RadialGradient {
  fn parse<'t>(
    input: &mut Parser<'i, 't>,
    vendor_prefix: VendorPrefix,
  ) -> Result<RadialGradient, ParseError<'i, ParserError<'i>>> {
    let shape = input.try_parse(EndingShape::parse).ok();
    let position = input
      .try_parse(|input| {
        input.expect_ident_matching("at")?;
        Position::parse(input)
      })
      .ok();

    if shape.is_some() || position.is_some() {
      input.expect_comma()?;
    }

    let items = parse_items(input)?;
    Ok(RadialGradient {
      shape: shape.unwrap_or_default(),
      position: position.unwrap_or(Position::center()),
      items,
      vendor_prefix,
    })
  }
}

impl RadialGradient {
  fn to_typst<W>(&self, dest: &mut Printer<W>, repeating: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.shape != EndingShape::default() {
      self.shape.to_typst(dest)?;
      if self.position.is_center() {
        dest.delim(',', false)?;
      } else {
        dest.write_char(' ')?;
      }
    }

    if !self.position.is_center() {
      dest.write_str("at ")?;
      self.position.to_typst(dest)?;
      dest.delim(',', false)?;
    }

    serialize_items(&self.items, repeating, dest)
  }
}

impl RadialGradient {
  fn get_fallback(&self, kind: ColorFallbackKind) -> RadialGradient {
    RadialGradient {
      shape: self.shape.clone(),
      position: self.position.clone(),
      items: self.items.iter().map(|item| item.get_fallback(kind)).collect(),
      vendor_prefix: self.vendor_prefix,
    }
  }
}

impl IsCompatible for RadialGradient {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.items.iter().all(|item| item.is_compatible(browsers))
  }
}

/// The direction of a CSS `linear-gradient()`.
///
/// See [LinearGradient](LinearGradient).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum LineDirection {
  /// An angle.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<Angle>"))]
  Angle(Angle),
  /// A horizontal position keyword, e.g. `left` or `right.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<HorizontalPositionKeyword>"))]
  Horizontal(HorizontalPositionKeyword),
  /// A vertical posision keyword, e.g. `top` or `bottom`.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<VerticalPositionKeyword>"))]
  Vertical(VerticalPositionKeyword),
  /// A corner, e.g. `bottom left` or `top right`.
  Corner {
    /// A horizontal position keyword, e.g. `left` or `right.
    horizontal: HorizontalPositionKeyword,
    /// A vertical posision keyword, e.g. `top` or `bottom`.
    vertical: VerticalPositionKeyword,
  },
}

impl LineDirection {
  fn parse<'i, 't>(
    input: &mut Parser<'i, 't>,
    is_prefixed: bool,
  ) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    // Spec allows unitless zero angles for gradients.
    // https://w3c.github.io/csswg-drafts/css-images-3/#linear-gradient-syntax
    if let Ok(angle) = input.try_parse(Angle::parse_with_unitless_zero) {
      return Ok(LineDirection::Angle(angle));
    }

    if !is_prefixed {
      input.expect_ident_matching("to")?;
    }

    if let Ok(x) = input.try_parse(HorizontalPositionKeyword::parse) {
      if let Ok(y) = input.try_parse(VerticalPositionKeyword::parse) {
        return Ok(LineDirection::Corner {
          horizontal: x,
          vertical: y,
        });
      }
      return Ok(LineDirection::Horizontal(x));
    }

    let y = VerticalPositionKeyword::parse(input)?;
    if let Ok(x) = input.try_parse(HorizontalPositionKeyword::parse) {
      return Ok(LineDirection::Corner {
        horizontal: x,
        vertical: y,
      });
    }
    Ok(LineDirection::Vertical(y))
  }
}

impl ToTypst for LineDirection {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use HorizontalPositionKeyword::*;
    use VerticalPositionKeyword::*;
    match self {
      // CSS defaults to top to bottom, whereas Typst defaults to left to right,
      // so we need to adjust the angle by 90 degrees.
      LineDirection::Angle(angle) => {
        dest.write_str("angle:")?;
        dest.whitespace()?;
        (Angle::Deg(-90.0) + angle.clone()).to_typst(dest)
      }
      LineDirection::Horizontal(k) => {
        dest.write_str("dir:")?;
        dest.whitespace()?;
        match k {
          Left => dest.write_str("rtl"),
          Right => dest.write_str("ltr"),
        }
      }
      LineDirection::Vertical(k) => {
        dest.write_str("dir:")?;
        dest.whitespace()?;
        match k {
          Top => dest.write_str("btt"),
          Bottom => dest.write_str("ttb"),
        }
      }
      LineDirection::Corner { horizontal, vertical } => {
        dest.write_str("angle:")?;
        dest.whitespace()?;
        match (horizontal, vertical) {
          (Right, Bottom) => dest.write_str("45deg"),
          (Left, Bottom) => dest.write_str("135deg"),
          (Left, Top) => dest.write_str("225deg"),
          (Right, Top) => dest.write_str("315deg"),
        }
      }
    }
  }
}

/// A `radial-gradient()` [ending shape](https://www.w3.org/TR/css-images-3/#valdef-radial-gradient-ending-shape).
///
/// See [RadialGradient](RadialGradient).
#[derive(Debug, Clone, PartialEq, Parse, ToTypst)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum EndingShape {
  // Note: Ellipse::parse MUST run before Circle::parse for this to be correct.
  /// An ellipse.
  Ellipse(Ellipse),
  /// A circle.
  Circle(Circle),
}

impl Default for EndingShape {
  fn default() -> EndingShape {
    EndingShape::Ellipse(Ellipse::Extent(ShapeExtent::FarthestCorner))
  }
}

/// A circle ending shape for a `radial-gradient()`.
///
/// See [RadialGradient](RadialGradient).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Circle {
  /// A circle with a specified radius.
  Radius(Length),
  /// A shape extent keyword.
  Extent(ShapeExtent),
}

impl<'i> Parse<'i> for Circle {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
      // The `circle` keyword is required. If it's not there, then it's an ellipse.
      input.expect_ident_matching("circle")?;
      return Ok(Circle::Extent(extent));
    }

    if let Ok(length) = input.try_parse(Length::parse) {
      // The `circle` keyword is optional if there is only a single length.
      // We are assuming here that Ellipse::parse ran first.
      let _ = input.try_parse(|input| input.expect_ident_matching("circle"));
      return Ok(Circle::Radius(length));
    }

    if input.try_parse(|input| input.expect_ident_matching("circle")).is_ok() {
      if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
        return Ok(Circle::Extent(extent));
      }

      if let Ok(length) = input.try_parse(Length::parse) {
        return Ok(Circle::Radius(length));
      }

      // If only the `circle` keyword was given, default to `farthest-corner`.
      return Ok(Circle::Extent(ShapeExtent::FarthestCorner));
    }

    return Err(input.new_error_for_next_token());
  }
}

impl ToTypst for Circle {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      Circle::Radius(r) => r.to_typst(dest),
      Circle::Extent(extent) => {
        dest.write_str("circle")?;
        if *extent != ShapeExtent::FarthestCorner {
          dest.write_char(' ')?;
          extent.to_typst(dest)?;
        }
        Ok(())
      }
    }
  }
}

/// An ellipse ending shape for a `radial-gradient()`.
///
/// See [RadialGradient](RadialGradient).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum Ellipse {
  /// An ellipse with a specified horizontal and vertical radius.
  Size {
    /// The x-radius of the ellipse.
    x: LengthPercentage,
    /// The y-radius of the ellipse.
    y: LengthPercentage,
  },
  /// A shape extent keyword.
  #[cfg_attr(feature = "serde", serde(with = "ValueWrapper::<ShapeExtent>"))]
  Extent(ShapeExtent),
}

impl<'i> Parse<'i> for Ellipse {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
      // The `ellipse` keyword is optional, but only if the `circle` keyword is not present.
      // If it is, then we'll re-parse as a circle.
      if input.try_parse(|input| input.expect_ident_matching("circle")).is_ok() {
        return Err(input.new_error_for_next_token());
      }
      let _ = input.try_parse(|input| input.expect_ident_matching("ellipse"));
      return Ok(Ellipse::Extent(extent));
    }

    if let Ok(x) = input.try_parse(LengthPercentage::parse) {
      let y = LengthPercentage::parse(input)?;
      // The `ellipse` keyword is optional if there are two lengths.
      let _ = input.try_parse(|input| input.expect_ident_matching("ellipse"));
      return Ok(Ellipse::Size { x, y });
    }

    if input.try_parse(|input| input.expect_ident_matching("ellipse")).is_ok() {
      if let Ok(extent) = input.try_parse(ShapeExtent::parse) {
        return Ok(Ellipse::Extent(extent));
      }

      if let Ok(x) = input.try_parse(LengthPercentage::parse) {
        let y = LengthPercentage::parse(input)?;
        return Ok(Ellipse::Size { x, y });
      }

      // Assume `farthest-corner` if only the `ellipse` keyword is present.
      return Ok(Ellipse::Extent(ShapeExtent::FarthestCorner));
    }

    return Err(input.new_error_for_next_token());
  }
}

impl ToTypst for Ellipse {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    // The `ellipse` keyword is optional, so we don't emit it.
    match self {
      Ellipse::Size { x, y } => {
        x.to_typst(dest)?;
        dest.write_char(' ')?;
        y.to_typst(dest)
      }
      Ellipse::Extent(extent) => extent.to_typst(dest),
    }
  }
}

enum_property! {
  /// A shape extent for a `radial-gradient()`.
  ///
  /// See [RadialGradient](RadialGradient).
  pub enum ShapeExtent {
    /// The closest side of the box to the gradient's center.
    ClosestSide,
    /// The farthest side of the box from the gradient's center.
    FarthestSide,
    /// The closest cornder of the box to the gradient's center.
    ClosestCorner,
    /// The farthest corner of the box from the gradient's center.
    FarthestCorner,
  }
}

/// A CSS [`conic-gradient()`](https://www.w3.org/TR/css-images-4/#conic-gradients) or `repeating-conic-gradient()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct ConicGradient {
  /// The angle of the gradient.
  pub angle: Angle,
  /// The position of the gradient.
  pub position: Position,
  /// The color stops and transition hints for the gradient.
  pub items: Vec<GradientItem<AnglePercentage>>,
}

impl ConicGradient {
  fn parse<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let angle = input.try_parse(|input| {
      input.expect_ident_matching("from")?;
      // Spec allows unitless zero angles for gradients.
      // https://w3c.github.io/csswg-drafts/css-images-4/#valdef-conic-gradient-angle
      Angle::parse_with_unitless_zero(input)
    });

    let position = input.try_parse(|input| {
      input.expect_ident_matching("at")?;
      Position::parse(input)
    });

    if angle.is_ok() || position.is_ok() {
      input.expect_comma()?;
    }

    let items = parse_items(input)?;
    Ok(ConicGradient {
      angle: angle.unwrap_or(Angle::Deg(0.0)),
      position: position.unwrap_or(Position::center()),
      items,
    })
  }
}

impl ConicGradient {
  fn to_typst<W>(&self, dest: &mut Printer<W>, repeating: bool) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    serialize_items(&self.items, repeating, dest)?;

    dest.delim(',', false)?;
    dest.write_str("angle:")?;
    dest.whitespace()?;
    // For CSS conic gradients, 0deg is from the top, but for Typst conic gradients,
    // 0deg is from the left. Have to add 90deg to start from top for Typst
    (self.angle.clone() + Angle::Deg(90.0)).to_typst(dest)?;

    if !self.position.is_center() {
      dest.delim(',', false)?;
      dest.write_str("center:")?;
      dest.whitespace()?;
      self.position.to_typst(dest)?;
    }

    Ok(())
  }
}

impl ConicGradient {
  fn get_fallback(&self, kind: ColorFallbackKind) -> ConicGradient {
    ConicGradient {
      angle: self.angle.clone(),
      position: self.position.clone(),
      items: self.items.iter().map(|item| item.get_fallback(kind)).collect(),
    }
  }
}

impl IsCompatible for ConicGradient {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    self.items.iter().all(|item| item.is_compatible(browsers))
  }
}

/// A [`<color-stop>`](https://www.w3.org/TR/css-images-4/#color-stop-syntax) within a gradient.
///
/// This type is generic, and may be either a [LengthPercentage](super::length::LengthPercentage)
/// or [Angle](super::angle::Angle) depending on what type of gradient it is within.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct ColorStop<D> {
  /// The color of the color stop.
  pub color: CssColor,
  /// The position of the color stop.
  pub position: Option<D>,
}

impl<'i, D: Parse<'i>> Parse<'i> for ColorStop<D> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let color = CssColor::parse(input)?;
    let position = input.try_parse(D::parse).ok();
    Ok(ColorStop { color, position })
  }
}

impl<D: ToTypst> ToTypst for ColorStop<D> {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    dest.write_char('(')?;
    self.color.to_typst(dest)?;
    dest.delim(',', false)?;
    if let Some(position) = &self.position {
      position.to_typst(dest)?;
    } else {
      dest.write_str("none")?;
    }
    dest.write_char(')')
  }
}

/// Either a color stop or interpolation hint within a gradient.
///
/// This type is generic, and items may be either a [LengthPercentage](super::length::LengthPercentage)
/// or [Angle](super::angle::Angle) depending on what type of gradient it is within.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub enum GradientItem<D> {
  /// A color stop.
  ColorStop(ColorStop<D>),
  /// A color interpolation hint.
  #[cfg_attr(
    feature = "serde",
    serde(
      bound(serialize = "D: serde::Serialize", deserialize = "D: serde::Deserialize<'de>"),
      with = "ValueWrapper::<D>"
    )
  )]
  Hint(D),
}

impl<D: ToTypst> ToTypst for GradientItem<D> {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      GradientItem::ColorStop(stop) => stop.to_typst(dest),
      GradientItem::Hint(hint) => {
        dest.write_str("(none")?;
        dest.delim(',', false)?;
        hint.to_typst(dest)?;
        dest.write_char(')')
      }
    }
  }
}

impl<D: Clone> GradientItem<D> {
  /// Returns the color fallback types needed for the given browser targets.
  pub fn get_necessary_fallbacks(&self, targets: Targets) -> ColorFallbackKind {
    match self {
      GradientItem::ColorStop(stop) => stop.color.get_necessary_fallbacks(targets),
      GradientItem::Hint(..) => ColorFallbackKind::empty(),
    }
  }

  /// Returns a fallback gradient item for the given color fallback type.
  pub fn get_fallback(&self, kind: ColorFallbackKind) -> GradientItem<D> {
    match self {
      GradientItem::ColorStop(stop) => GradientItem::ColorStop(ColorStop {
        color: stop.color.get_fallback(kind),
        position: stop.position.clone(),
      }),
      GradientItem::Hint(..) => self.clone(),
    }
  }
}

impl<D> IsCompatible for GradientItem<D> {
  fn is_compatible(&self, browsers: Browsers) -> bool {
    match self {
      GradientItem::ColorStop(c) => c.color.is_compatible(browsers),
      GradientItem::Hint(..) => compat::Feature::GradientInterpolationHints.is_compatible(browsers),
    }
  }
}

fn parse_items<'i, 't, D: Parse<'i>>(
  input: &mut Parser<'i, 't>,
) -> Result<Vec<GradientItem<D>>, ParseError<'i, ParserError<'i>>> {
  let mut items = Vec::new();
  let mut seen_stop = false;

  loop {
    input.parse_until_before(Delimiter::Comma, |input| {
      if seen_stop {
        if let Ok(hint) = input.try_parse(D::parse) {
          seen_stop = false;
          items.push(GradientItem::Hint(hint));
          return Ok(());
        }
      }

      let stop = ColorStop::parse(input)?;

      if let Ok(position) = input.try_parse(D::parse) {
        let color = stop.color.clone();
        items.push(GradientItem::ColorStop(stop));

        items.push(GradientItem::ColorStop(ColorStop {
          color,
          position: Some(position),
        }))
      } else {
        items.push(GradientItem::ColorStop(stop));
      }

      seen_stop = true;
      Ok(())
    })?;

    match input.next() {
      Err(err) => {
        // An error indicates that the parser has reached the end of the gradient
        // items. The last item should be a color stop, so if it's a hint, then
        // the gradient is malformed and we propagate the error.
        if matches!(items.last(), Some(GradientItem::Hint(_))) {
          return Err(err.into());
        }
        break;
      }
      Ok(Token::Comma) => continue,
      _ => unreachable!(),
    }
  }

  Ok(items)
}

/// Positions the first and last items in a gradient at 0% and 100% (respectively) when
/// able.
/// 
/// Does step 1 from https://drafts.csswg.org/css-images-4/#color-stop-fixup:
/// 
/// > If the first color stop does not have a position, set its position to 0%. If
/// > the last color stop does not have a position, set its position to 100%.
///
/// There are also some cases where the first/last item will be cloned with a new
/// position of 0%/100% when the gradient isn't repeating:
/// 
/// - The first item will be cloned with 0% if its position is positive.
/// - The last item will be cloned with 100% if its position is a percentage < 100%.
/// 
/// Lastly, if the first item `is_zero()`, it will be replaced by 0%.
fn ensure_boundary_stop<D: std::cmp::PartialEq<D> + std::cmp::PartialOrd<D> + Clone + TrySign + Zero>(
  items: &mut Vec<GradientItem<DimensionPercentage<D>>>,
  is_start: bool,
  repeating: bool,
) {
  let index = if is_start { 0 } else { items.len() - 1 };
  let boundary = if is_start { 0.0 } else { 1.0 };
  let boundary = DimensionPercentage::Percentage(Percentage(boundary));
  /*
   * Here's the rules for handling the first position:
   *
   * - If no position or dim position == 0, replace with 0%
   * - If not repeating
   *   - If position > 0, clone with 0%
   *   - If position < 0, do nothing
   *
   * Here's the rules for handling the last position:
   *
   * - If no position, replace with 100%
   * - If not repeating
   *   - If a per position < 100%, clone with 100%
   *   - If a per position > 100% or a dim position, do nothing
   */
  match &mut items[index] {
    GradientItem::ColorStop(stop) => {
      match &stop.position {
        Some(position) if *position == boundary => {}
        // This is separate from the following pattern so that it applies to
        // repeating gradients as well.
        Some(position) if is_start && position.is_zero() => {
          // replace with percentage so we don't have to in Typst
          stop.position = Some(boundary);
        }
        // If the gradient is repeating, the only change we want to make is setting
        // the position if it's missing, hence the guard here
        Some(position) if !repeating => {
          if is_start {
            if position.is_sign_positive() {
              let new_item = GradientItem::ColorStop(ColorStop {
                color: stop.color.clone(),
                position: Some(boundary),
              });
              items.insert(0, new_item);
            }
          // This is false if position is not a percentage (so nothing will be added)
          } else if *position <= boundary {
            let new_item = GradientItem::ColorStop(ColorStop {
              color: stop.color.clone(),
              position: Some(boundary),
            });
            items.push(new_item);
          }
        }
        None => {
          stop.position = Some(boundary);
        }
        _ => {}
      }
    }
    // parse_items ensures the first and last items aren't hints.
    GradientItem::Hint(_) => unreachable!(),
  };
}


/// Ensures items with positions positions are in ascending order.
/// 
/// Does step 2 from https://drafts.csswg.org/css-images-4/#color-stop-fixup:
/// 
/// > If a color stop or transition hint has a position that is less than the specified
/// > position of any color stop or transition hint before it in the list, set its
/// > position to be equal to the largest specified position of any color stop or
/// > transition hint before it. 
fn clamp_positions<D: std::cmp::PartialOrd<D> + Clone>(items: &mut Vec<GradientItem<DimensionPercentage<D>>>) {
  let mut iter = items.iter_mut();
  let mut max_position = match iter.next() {
    Some(GradientItem::ColorStop(ColorStop {
      position: Some(pos), ..
    }))
    | Some(GradientItem::Hint(pos)) => pos.clone(),
    _ => unreachable!(),
  };
  for item in items.iter_mut() {
    match item {
      // We ignore DimensionPercentage::Dimension(_) because they are
      // absolute units, so we can't clamp them to 0% and 100%.
      GradientItem::ColorStop(ColorStop {
        position: Some(position @ DimensionPercentage::Percentage(_)),
        ..
      })
      | GradientItem::Hint(position @ DimensionPercentage::Percentage(_)) => {
        if *position < max_position {
          *position = max_position.clone();
        } else {
          max_position = position.clone();
        }
      }
      _ => {}
    }
  }
}

/// Does step 1 and 2 of color stop fixup from
/// https://drafts.csswg.org/css-images-4/#color-stop-fixup.
fn fix_items<
  D: std::cmp::PartialEq<D>
    + std::ops::Mul<f32, Output = D>
    + TrySign
    + Clone
    + std::cmp::PartialOrd<D>
    + TryAdd<D>
    + Zero
    + std::fmt::Debug,
>(
  items: &Vec<GradientItem<DimensionPercentage<D>>>,
  repeating: bool,
) -> Vec<GradientItem<DimensionPercentage<D>>> {
  if items.is_empty() {
    return vec![]; // This should never happen, but just in case.
  }

  let mut items = items.clone();
  ensure_boundary_stop(&mut items, true, repeating);
  ensure_boundary_stop(&mut items, false, repeating);
  clamp_positions(&mut items);

  items
}

fn serialize_items<
  D: ToTypst
    + std::cmp::PartialEq<D>
    + std::ops::Mul<f32, Output = D>
    + TrySign
    + Clone
    + std::fmt::Debug
    + std::cmp::PartialOrd<D>
    + TryAdd<D>
    + Zero,
  W,
>(
  items: &Vec<GradientItem<DimensionPercentage<D>>>,
  repeating: bool,
  dest: &mut Printer<W>,
) -> Result<(), PrinterError>
where
  W: std::fmt::Write,
{
  let items = fix_items(items, repeating);
  let mut first = true;
  for item in items {
    // Skip useless hints
    if item == GradientItem::Hint(DimensionPercentage::Percentage(Percentage(0.5))) {
      continue;
    }

    if first {
      first = false;
    } else {
      dest.delim(',', false)?;
    }
    item.to_typst(dest)?;
  }
  Ok(())
}

/// A legacy `-webkit-gradient()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "kind", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum WebKitGradient {
  /// A linear `-webkit-gradient()`.
  Linear {
    /// The starting point of the gradient.
    from: WebKitGradientPoint,
    /// The ending point of the gradient.
    to: WebKitGradientPoint,
    /// The color stops in the gradient.
    stops: Vec<WebKitColorStop>,
  },
  /// A radial `-webkit-gradient()`.
  Radial {
    /// The starting point of the gradient.
    from: WebKitGradientPoint,
    /// The starting radius of the gradient.
    r0: CSSNumber,
    /// The ending point of the gradient.
    to: WebKitGradientPoint,
    /// The ending radius of the gradient.
    r1: CSSNumber,
    /// The color stops in the gradient.
    stops: Vec<WebKitColorStop>,
  },
}

impl<'i> Parse<'i> for WebKitGradient {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let ident = input.expect_ident_cloned()?;
    input.expect_comma()?;

    match_ignore_ascii_case! { &ident,
      "linear" => {
        let from = WebKitGradientPoint::parse(input)?;
        input.expect_comma()?;
        let to = WebKitGradientPoint::parse(input)?;
        input.expect_comma()?;
        let stops = input.parse_comma_separated(WebKitColorStop::parse)?;
        Ok(WebKitGradient::Linear {
          from,
          to,
          stops
        })
      },
      "radial" => {
        let from = WebKitGradientPoint::parse(input)?;
        input.expect_comma()?;
        let r0 = CSSNumber::parse(input)?;
        input.expect_comma()?;
        let to = WebKitGradientPoint::parse(input)?;
        input.expect_comma()?;
        let r1 = CSSNumber::parse(input)?;
        input.expect_comma()?;
        let stops = input.parse_comma_separated(WebKitColorStop::parse)?;
        Ok(WebKitGradient::Radial {
          from,
          r0,
          to,
          r1,
          stops
        })
      },
      _ => Err(location.new_unexpected_token_error(cssparser::Token::Ident(ident.clone())))
    }
  }
}

impl ToTypst for WebKitGradient {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    match self {
      WebKitGradient::Linear { from, to, stops } => {
        dest.write_str("linear")?;
        dest.delim(',', false)?;
        from.to_typst(dest)?;
        dest.delim(',', false)?;
        to.to_typst(dest)?;
        for stop in stops {
          dest.delim(',', false)?;
          stop.to_typst(dest)?;
        }
        Ok(())
      }
      WebKitGradient::Radial {
        from,
        r0,
        to,
        r1,
        stops,
      } => {
        dest.write_str("radial")?;
        dest.delim(',', false)?;
        from.to_typst(dest)?;
        dest.delim(',', false)?;
        r0.to_typst(dest)?;
        dest.delim(',', false)?;
        to.to_typst(dest)?;
        dest.delim(',', false)?;
        r1.to_typst(dest)?;
        for stop in stops {
          dest.delim(',', false)?;
          stop.to_typst(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl WebKitGradient {
  fn get_fallback(&self, kind: ColorFallbackKind) -> WebKitGradient {
    let stops = match self {
      WebKitGradient::Linear { stops, .. } => stops,
      WebKitGradient::Radial { stops, .. } => stops,
    };

    let stops = stops.iter().map(|stop| stop.get_fallback(kind)).collect();

    match self {
      WebKitGradient::Linear { from, to, .. } => WebKitGradient::Linear {
        from: from.clone(),
        to: to.clone(),
        stops,
      },
      WebKitGradient::Radial { from, r0, to, r1, .. } => WebKitGradient::Radial {
        from: from.clone(),
        r0: *r0,
        to: to.clone(),
        r1: *r1,
        stops,
      },
    }
  }
}

/// A color stop within a legacy `-webkit-gradient()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
pub struct WebKitColorStop {
  /// The color of the color stop.
  pub color: CssColor,
  /// The position of the color stop.
  pub position: CSSNumber,
}

impl<'i> Parse<'i> for WebKitColorStop {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let location = input.current_source_location();
    let function = input.expect_function()?.clone();
    input.parse_nested_block(|input| {
      let position = match_ignore_ascii_case! { &function,
        "color-stop" => {
          let p = NumberOrPercentage::parse(input)?;
          input.expect_comma()?;
          (&p).into()
        },
        "from" => 0.0,
        "to" => 1.0,
        _ => return Err(location.new_unexpected_token_error(cssparser::Token::Ident(function.clone())))
      };
      let color = CssColor::parse(input)?;
      Ok(WebKitColorStop { color, position })
    })
  }
}

impl ToTypst for WebKitColorStop {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    if self.position == 0.0 {
      dest.write_str("from(")?;
      self.color.to_typst(dest)?;
    } else if self.position == 1.0 {
      dest.write_str("to(")?;
      self.color.to_typst(dest)?;
    } else {
      dest.write_str("color-stop(")?;
      self.position.to_typst(dest)?;
      dest.delim(',', false)?;
      self.color.to_typst(dest)?;
    }
    dest.write_char(')')
  }
}

impl WebKitColorStop {
  fn get_fallback(&self, kind: ColorFallbackKind) -> WebKitColorStop {
    WebKitColorStop {
      color: self.color.get_fallback(kind),
      position: self.position,
    }
  }
}

/// An x/y position within a legacy `-webkit-gradient()`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub struct WebKitGradientPoint {
  /// The x-position.
  pub x: WebKitGradientPointComponent<HorizontalPositionKeyword>,
  /// The y-position.
  pub y: WebKitGradientPointComponent<VerticalPositionKeyword>,
}

impl<'i> Parse<'i> for WebKitGradientPoint {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    let x = WebKitGradientPointComponent::parse(input)?;
    let y = WebKitGradientPointComponent::parse(input)?;
    Ok(WebKitGradientPoint { x, y })
  }
}

impl ToTypst for WebKitGradientPoint {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    self.x.to_typst(dest)?;
    dest.write_char(' ')?;
    self.y.to_typst(dest)
  }
}

/// A keyword or number within a [WebKitGradientPoint](WebKitGradientPoint).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "visitor", derive(Visit))]
#[cfg_attr(
  feature = "serde",
  derive(serde::Serialize, serde::Deserialize),
  serde(tag = "type", content = "value", rename_all = "kebab-case")
)]
#[cfg_attr(feature = "jsonschema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "into_owned", derive(static_self::IntoOwned))]
pub enum WebKitGradientPointComponent<S> {
  /// The `center` keyword.
  Center,
  /// A number or percentage.
  Number(NumberOrPercentage),
  /// A side keyword.
  Side(S),
}

impl<'i, S: Parse<'i>> Parse<'i> for WebKitGradientPointComponent<S> {
  fn parse<'t>(input: &mut Parser<'i, 't>) -> Result<Self, ParseError<'i, ParserError<'i>>> {
    if input.try_parse(|i| i.expect_ident_matching("center")).is_ok() {
      return Ok(WebKitGradientPointComponent::Center);
    }

    if let Ok(lp) = input.try_parse(NumberOrPercentage::parse) {
      return Ok(WebKitGradientPointComponent::Number(lp));
    }

    let keyword = S::parse(input)?;
    Ok(WebKitGradientPointComponent::Side(keyword))
  }
}

impl<S: ToTypst + Clone + Into<LengthPercentage>> ToTypst for WebKitGradientPointComponent<S> {
  fn to_typst<W>(&self, dest: &mut Printer<W>) -> Result<(), PrinterError>
  where
    W: std::fmt::Write,
  {
    use WebKitGradientPointComponent::*;
    match &self {
      Center => {
        if dest.minify {
          dest.write_str("50%")
        } else {
          dest.write_str("center")
        }
      }
      Number(lp) => {
        if matches!(lp, NumberOrPercentage::Percentage(Percentage(p)) if *p == 0.0) {
          dest.write_char('0')
        } else {
          lp.to_typst(dest)
        }
      }
      Side(s) => {
        if dest.minify {
          let lp: LengthPercentage = s.clone().into();
          lp.to_typst(dest)?;
        } else {
          s.to_typst(dest)?;
        }
        Ok(())
      }
    }
  }
}

impl<S: Clone> WebKitGradientPointComponent<S> {
  /// Attempts to convert a standard position to a webkit gradient point.
  fn from_position(pos: &PositionComponent<S>) -> Result<WebKitGradientPointComponent<S>, ()> {
    match pos {
      PositionComponent::Center => Ok(WebKitGradientPointComponent::Center),
      PositionComponent::Length(len) => {
        Ok(WebKitGradientPointComponent::Number(match len {
          LengthPercentage::Percentage(p) => NumberOrPercentage::Percentage(p.clone()),
          LengthPercentage::Dimension(d) => {
            // Webkit gradient points can only be specified in pixels.
            if let Some(px) = d.to_px() {
              NumberOrPercentage::Number(px)
            } else {
              return Err(());
            }
          }
          _ => return Err(()),
        }))
      }
      PositionComponent::Side { side, offset } => {
        if offset.is_some() {
          return Err(());
        }
        Ok(WebKitGradientPointComponent::Side(side.clone()))
      }
    }
  }
}

impl WebKitGradient {
  /// Attempts to convert a standard gradient to a legacy -webkit-gradient()
  pub fn from_standard(gradient: &Gradient) -> Result<WebKitGradient, ()> {
    match gradient {
      Gradient::Linear(linear) => {
        // Convert from line direction to a from and to point, if possible.
        let (from, to) = match &linear.direction {
          LineDirection::Horizontal(horizontal) => match horizontal {
            HorizontalPositionKeyword::Left => ((1.0, 0.0), (0.0, 0.0)),
            HorizontalPositionKeyword::Right => ((0.0, 0.0), (1.0, 0.0)),
          },
          LineDirection::Vertical(vertical) => match vertical {
            VerticalPositionKeyword::Top => ((0.0, 1.0), (0.0, 0.0)),
            VerticalPositionKeyword::Bottom => ((0.0, 0.0), (0.0, 1.0)),
          },
          LineDirection::Corner { horizontal, vertical } => match (horizontal, vertical) {
            (HorizontalPositionKeyword::Left, VerticalPositionKeyword::Top) => ((1.0, 1.0), (0.0, 0.0)),
            (HorizontalPositionKeyword::Left, VerticalPositionKeyword::Bottom) => ((1.0, 0.0), (0.0, 1.0)),
            (HorizontalPositionKeyword::Right, VerticalPositionKeyword::Top) => ((0.0, 1.0), (1.0, 0.0)),
            (HorizontalPositionKeyword::Right, VerticalPositionKeyword::Bottom) => ((0.0, 0.0), (1.0, 1.0)),
          },
          LineDirection::Angle(angle) => {
            let degrees = angle.to_degrees();
            if degrees == 0.0 {
              ((0.0, 1.0), (0.0, 0.0))
            } else if degrees == 90.0 {
              ((0.0, 0.0), (1.0, 0.0))
            } else if degrees == 180.0 {
              ((0.0, 0.0), (0.0, 1.0))
            } else if degrees == 270.0 {
              ((1.0, 0.0), (0.0, 0.0))
            } else {
              return Err(());
            }
          }
        };

        Ok(WebKitGradient::Linear {
          from: WebKitGradientPoint {
            x: WebKitGradientPointComponent::Number(NumberOrPercentage::Percentage(Percentage(from.0))),
            y: WebKitGradientPointComponent::Number(NumberOrPercentage::Percentage(Percentage(from.1))),
          },
          to: WebKitGradientPoint {
            x: WebKitGradientPointComponent::Number(NumberOrPercentage::Percentage(Percentage(to.0))),
            y: WebKitGradientPointComponent::Number(NumberOrPercentage::Percentage(Percentage(to.1))),
          },
          stops: convert_stops_to_webkit(&linear.items)?,
        })
      }
      Gradient::Radial(radial) => {
        // Webkit radial gradients are always circles, not ellipses, and must be specified in pixels.
        let radius = match &radial.shape {
          EndingShape::Circle(Circle::Radius(radius)) => {
            if let Some(r) = radius.to_pt() {
              r
            } else {
              return Err(());
            }
          }
          _ => return Err(()),
        };

        let x = WebKitGradientPointComponent::from_position(&radial.position.x)?;
        let y = WebKitGradientPointComponent::from_position(&radial.position.y)?;
        let point = WebKitGradientPoint { x, y };
        Ok(WebKitGradient::Radial {
          from: point.clone(),
          r0: 0.0,
          to: point,
          r1: radius,
          stops: convert_stops_to_webkit(&radial.items)?,
        })
      }
      _ => Err(()),
    }
  }
}

fn convert_stops_to_webkit(items: &Vec<GradientItem<LengthPercentage>>) -> Result<Vec<WebKitColorStop>, ()> {
  let mut stops = Vec::with_capacity(items.len());
  for (i, item) in items.iter().enumerate() {
    match item {
      GradientItem::ColorStop(stop) => {
        // webkit stops must always be percentage based, not length based.
        let position = if let Some(pos) = &stop.position {
          if let LengthPercentage::Percentage(position) = pos {
            position.0
          } else {
            return Err(());
          }
        } else if i == 0 {
          0.0
        } else if i == items.len() - 1 {
          1.0
        } else {
          return Err(());
        };

        stops.push(WebKitColorStop {
          color: stop.color.clone(),
          position,
        })
      }
      _ => return Err(()),
    }
  }

  Ok(stops)
}
