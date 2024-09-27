use lightningcss::{
  declaration::DeclarationBlock,
  properties::{Property, PropertyId},
  stylesheet::{ParserOptions, PrinterOptions},
  traits::ToTypst,
  vendor_prefix::VendorPrefix,
};

fn get_test(decls: &str, property_id: PropertyId, expected: Option<(&str, bool)>) {
  let decls = DeclarationBlock::parse_string(decls, ParserOptions::default()).unwrap();
  let v = decls.get(&property_id);
  if let Some((expected, important)) = expected {
    let (value, is_important) = v.unwrap();
    assert_eq!(
      *value,
      Property::parse_string(property_id, expected, ParserOptions::default()).unwrap()
    );
    assert_eq!(is_important, important);
  } else {
    assert_eq!(v, None)
  }
}

#[test]
fn test_get() {
  get_test("color: red", PropertyId::Color, Some(("red", false)));
  get_test("color: red !important", PropertyId::Color, Some(("red", true)));
  get_test("color: green; color: red", PropertyId::Color, Some(("red", false)));
  get_test(
    r#"
    margin-top: 5pt;
    margin-bottom: 5pt;
    margin-left: 5pt;
    margin-right: 5pt;
    "#,
    PropertyId::Margin,
    Some(("5pt", false)),
  );
  get_test(
    r#"
    margin-top: 5pt;
    margin-bottom: 5pt;
    margin-left: 6pt;
    margin-right: 6pt;
    "#,
    PropertyId::Margin,
    Some(("5pt 6pt", false)),
  );
  get_test(
    r#"
    margin-top: 5pt;
    margin-bottom: 5pt;
    margin-left: 6pt;
    margin-right: 6pt;
    "#,
    PropertyId::Margin,
    Some(("5pt 6pt", false)),
  );
  get_test(
    r#"
    margin-top: 5pt;
    margin-bottom: 5pt;
    "#,
    PropertyId::Margin,
    None,
  );
  get_test(
    r#"
    margin-top: 5pt;
    margin-bottom: 5pt;
    margin-left: 5pt !important;
    margin-right: 5pt;
    "#,
    PropertyId::Margin,
    None,
  );
  get_test(
    r#"
    margin-top: 5pt !important;
    margin-bottom: 5pt !important;
    margin-left: 5pt !important;
    margin-right: 5pt !important;
    "#,
    PropertyId::Margin,
    Some(("5pt", true)),
  );
  get_test(
    "margin: 5pt 6pt 7pt 8pt",
    PropertyId::Margin,
    Some(("5pt 6pt 7pt 8pt", false)),
  );
  get_test("margin: 5pt 6pt 7pt 8pt", PropertyId::MarginTop, Some(("5pt", false)));
  get_test(
    r#"
    border: 1pt solid red;
    border-color: green;
    "#,
    PropertyId::Border,
    Some(("1pt solid green", false)),
  );
  get_test(
    r#"
    border: 1pt solid red;
    border-left-color: green;
    "#,
    PropertyId::Border,
    None,
  );
  get_test("background: red", PropertyId::Background, Some(("red", false)));
  get_test("background: red", PropertyId::BackgroundColor, Some(("red", false)));
  get_test(
    "background: red url(foo.png)",
    PropertyId::BackgroundColor,
    Some(("red", false)),
  );
  get_test(
    "background: url(foo.png), url(bar.png) red",
    PropertyId::BackgroundColor,
    Some(("red", false)),
  );
  get_test(
    "background: url(foo.png) green, url(bar.png) red",
    PropertyId::BackgroundColor,
    Some(("red", false)),
  );
  get_test(
    "background: linear-gradient(red, green)",
    PropertyId::BackgroundImage,
    Some(("linear-gradient(red, green)", false)),
  );
  get_test(
    "background: linear-gradient(red, green), linear-gradient(#fff, #000)",
    PropertyId::BackgroundImage,
    Some(("linear-gradient(red, green), linear-gradient(#fff, #000)", false)),
  );
  get_test(
    "background: linear-gradient(red, green) repeat-x, linear-gradient(#fff, #000) repeat-y",
    PropertyId::BackgroundImage,
    Some(("linear-gradient(red, green), linear-gradient(#fff, #000)", false)),
  );
  get_test(
    "background: linear-gradient(red, green) repeat-x, linear-gradient(#fff, #000) repeat-y",
    PropertyId::BackgroundRepeat,
    Some(("repeat-x, repeat-y", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position-x: 20pt;
    background-position-y: 10pt;
    background-size: 50pt 100pt;
    background-repeat: repeat no-repeat;
    "#,
    PropertyId::Background,
    Some(("linear-gradient(red, green) 20pt 10pt / 50pt 100pt repeat-x", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position-x: 20pt;
    background-position-y: 10pt !important;
    background-size: 50pt 100pt;
    background-repeat: repeat no-repeat;
    "#,
    PropertyId::Background,
    None,
  );
  get_test(
    r#"
    background: linear-gradient(red, green), linear-gradient(#fff, #000) gray;
    background-position-x: right 20pt, 10pt;
    background-position-y: top 20pt, 15pt;
    background-size: 50pt 50pt, auto;
    background-repeat: repeat no-repeat, no-repeat;
    "#,
    PropertyId::Background,
    Some(("linear-gradient(red, green) right 20pt top 20pt / 50pt 50pt repeat-x, gray linear-gradient(#fff, #000) 10pt 15pt no-repeat", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position-x: right 20pt, 10pt;
    background-position-y: top 20pt, 15pt;
    background-size: 50pt 50pt, auto;
    background-repeat: repeat no-repeat, no-repeat;
    "#,
    PropertyId::Background,
    None,
  );
  get_test(
    r#"
    background: linear-gradient(red, green);
    background-position: 20pt 10pt;
    background-size: 50pt 100pt;
    background-repeat: repeat no-repeat;
    "#,
    PropertyId::Background,
    Some(("linear-gradient(red, green) 20pt 10pt / 50pt 100pt repeat-x", false)),
  );
  get_test(
    r#"
    background-position-x: 20pt;
    background-position-y: 10pt;
    "#,
    PropertyId::BackgroundPosition,
    Some(("20pt 10pt", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green) 20pt 10pt;
    "#,
    PropertyId::BackgroundPosition,
    Some(("20pt 10pt", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green) 20pt 10pt;
    "#,
    PropertyId::BackgroundPositionX,
    Some(("20pt", false)),
  );
  get_test(
    r#"
    background: linear-gradient(red, green) 20pt 10pt;
    "#,
    PropertyId::BackgroundPositionY,
    Some(("10pt", false)),
  );
  get_test(
    "mask-border: linear-gradient(red, green) 25",
    PropertyId::MaskBorderSource,
    Some(("linear-gradient(red, green)", false)),
  );
  get_test("grid-area: a / b / c / d", PropertyId::GridRowStart, Some(("a", false)));
  get_test("grid-area: a / b / c / d", PropertyId::GridRowEnd, Some(("c", false)));
  get_test("grid-area: a / b / c / d", PropertyId::GridRow, Some(("a / c", false)));
  get_test(
    "grid-area: a / b / c / d",
    PropertyId::GridColumn,
    Some(("b / d", false)),
  );
  get_test(
    r#"
    grid-template-rows: auto 1fr;
    grid-template-columns: auto 1fr auto;
    grid-template-areas: none;
    "#,
    PropertyId::GridTemplate,
    Some(("auto 1fr / auto 1fr auto", false)),
  );
  get_test(
    r#"
    grid-template-areas: ". a a ."
        ". b b .";
    grid-template-rows: auto 1fr;
    grid-template-columns: 10pt 1fr 1fr 10pt;
    "#,
    PropertyId::GridTemplate,
    Some((
      r#"
      ". a a ."
      ". b b ." 1fr
      / 10pt 1fr 1fr 10pt
      "#,
      false,
    )),
  );
  get_test(
    r#"
    grid-template-areas: "a a a"
                          "b b b";
    grid-template-columns: repeat(3, 1fr);
    grid-template-rows: auto 1fr;
    "#,
    PropertyId::GridTemplate,
    None,
  );
  get_test(
    r#"
    grid-template-areas: "a a a"
                         "b b b";
    grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
    grid-template-columns: auto 1fr auto;
    grid-auto-flow: row;
    grid-auto-rows: auto;
    grid-auto-columns: auto;
    "#,
    PropertyId::Grid,
    Some((
      r#"
      [header-top] "a a a" [header-bottom]
      [main-top] "b b b" 1fr [main-bottom]
      / auto 1fr auto
      "#,
      false,
    )),
  );
  get_test(
    r#"
    grid-template-areas: "a a a"
                         "b b b";
    grid-template-rows: [header-top] auto [header-bottom main-top] 1fr [main-bottom];
    grid-template-columns: auto 1fr auto;
    grid-auto-flow: column;
    grid-auto-rows: 1fr;
    grid-auto-columns: 1fr;
    "#,
    PropertyId::Grid,
    None,
  );
  get_test(
    r#"
    flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::None),
    Some(("row wrap", false)),
  );
  get_test(
    r#"
    -webkit-flex-direction: row;
    -webkit-flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::WebKit),
    Some(("row wrap", false)),
  );
  get_test(
    r#"
    flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::WebKit),
    None,
  );
  get_test(
    r#"
    -webkit-flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::WebKit),
    None,
  );
  get_test(
    r#"
    -webkit-flex-direction: row;
    flex-wrap: wrap;
    "#,
    PropertyId::FlexFlow(VendorPrefix::None),
    None,
  );
  get_test(
    r#"
    -webkit-flex-flow: row;
    "#,
    PropertyId::FlexDirection(VendorPrefix::WebKit),
    Some(("row", false)),
  );
  get_test(
    r#"
    -webkit-flex-flow: row;
    "#,
    PropertyId::FlexDirection(VendorPrefix::None),
    None,
  );
}

fn set_test(orig: &str, property: &str, value: &str, important: bool, expected: &str) {
  let mut decls = DeclarationBlock::parse_string(orig, ParserOptions::default()).unwrap();
  decls.set(
    Property::parse_string(property.into(), value, ParserOptions::default()).unwrap(),
    important,
  );
  assert_eq!(decls.to_typst_string(PrinterOptions::default()).unwrap(), expected);
}

#[test]
fn test_set() {
  set_test("color: red", "color", "green", false, "color: rgb(0, 128, 0)");
  set_test("color: red !important", "color", "green", false, "color: rgb(0, 128, 0)");
  set_test("color: red", "color", "green", true, "color: rgb(0, 128, 0) !important");
  set_test("margin: 5pt", "margin", "10pt", false, "margin: 10pt");
  set_test("margin: 5pt", "margin-top", "8pt", false, "margin: 8pt 5pt 5pt");
  set_test(
    "margin: 5pt",
    "margin-inline-start",
    "8pt",
    false,
    "margin: 5pt; margin-inline-start: 8pt",
  );
  set_test(
    "margin-inline-start: 5pt; margin-top: 10pt",
    "margin-inline-start",
    "8pt",
    false,
    "margin-inline-start: 5pt; margin-top: 10pt; margin-inline-start: 8pt",
  );
  set_test(
    "margin: 5pt; margin-inline-start: 8pt",
    "margin-left",
    "10pt",
    false,
    "margin: 5pt; margin-inline-start: 8pt; margin-left: 10pt",
  );
  set_test(
    "border: 1pt solid red",
    "border-right",
    "1pt solid green",
    false,
    "border: 1pt solid rgb(255, 0, 0); border-right: 1pt solid rgb(0, 128, 0)",
  );
  set_test(
    "border: 1pt solid red",
    "border-right-color",
    "green",
    false,
    "border: 1pt solid rgb(255, 0, 0); border-right-color: rgb(0, 128, 0)",
  );
  set_test(
    "animation: foo 2s",
    "animation-name",
    "foo, bar",
    false,
    "animation: 2s foo; animation-name: foo, bar",
  );
  set_test("animation: foo 2s", "animation-name", "bar", false, "animation: 2s bar");
  set_test(
    "background: linear-gradient(red, green)",
    "background-position-x",
    "20pt",
    false,
    "background: linear-gradient((rgb(255, 0, 0), 0%), (rgb(0, 128, 0), 100%), dir: ttb) 20pt 0pt",
  );
  set_test(
    "background: linear-gradient(red, green)",
    "background-position",
    "20pt 10pt",
    false,
    "background: linear-gradient((rgb(255, 0, 0), 0%), (rgb(0, 128, 0), 100%), dir: ttb) 20pt 10pt",
  );
  set_test(
    "flex-flow: row wrap",
    "flex-direction",
    "column",
    false,
    "flex-flow: column wrap",
  );
  set_test(
    "-webkit-flex-flow: row wrap",
    "-webkit-flex-direction",
    "column",
    false,
    "-webkit-flex-flow: column wrap",
  );
  set_test(
    "flex-flow: row wrap",
    "-webkit-flex-direction",
    "column",
    false,
    "flex-flow: wrap; -webkit-flex-direction: column",
  );
}

fn remove_test(orig: &str, property_id: PropertyId, expected: &str) {
  let mut decls = DeclarationBlock::parse_string(orig, ParserOptions::default()).unwrap();
  decls.remove(&property_id);
  assert_eq!(decls.to_typst_string(PrinterOptions::default()).unwrap(), expected);
}

#[test]
fn test_remove() {
  remove_test("margin-top: 10pt", PropertyId::MarginTop, "");
  remove_test(
    "margin-top: 10pt; margin-left: 5pt",
    PropertyId::MarginTop,
    "margin-left: 5pt",
  );
  remove_test(
    "margin-top: 10pt !important; margin-left: 5pt",
    PropertyId::MarginTop,
    "margin-left: 5pt",
  );
  remove_test(
    "margin: 10pt",
    PropertyId::MarginTop,
    "margin-right: 10pt; margin-bottom: 10pt; margin-left: 10pt",
  );
  remove_test("margin: 10pt", PropertyId::Margin, "");
  remove_test(
    "margin-top: 10pt; margin-right: 10pt; margin-bottom: 10pt; margin-left: 10pt",
    PropertyId::Margin,
    "",
  );
  remove_test(
    "flex-flow: column wrap",
    PropertyId::FlexDirection(VendorPrefix::None),
    "flex-wrap: wrap",
  );
  remove_test(
    "flex-flow: column wrap",
    PropertyId::FlexDirection(VendorPrefix::WebKit),
    "flex-flow: column wrap",
  );
  remove_test(
    "-webkit-flex-flow: column wrap",
    PropertyId::FlexDirection(VendorPrefix::WebKit),
    "-webkit-flex-wrap: wrap",
  );
}
