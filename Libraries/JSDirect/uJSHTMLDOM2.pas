unit uJSHTMLDOM2;

interface

uses
  xmldom,
  htmldom2,
  uJSXMLDom;

type
  TjsHTMLElement = class(TjsDOMElement,IHTMLElement)
  protected
    function Get_className : DOMString;
    procedure Set_className(AValue : DOMString);
    function Get_dir : DOMString;
    procedure Set_dir(AValue : DOMString);
    function Get_id : DOMString;
    procedure Set_id(AValue : DOMString);
    function Get_lang : DOMString;
    procedure Set_lang(AValue : DOMString);
    function Get_title : DOMString;
    procedure Set_title(AValue : DOMString);
    function item(index : LongInt) : IDOMNode;
    function namedItem(name : DOMString) : IDOMNode;
  end;

  TjsHTMLCollection = class(TjsDOMObject, IHTMLCollection)
  protected
    function item(index : LongInt) : IDOMNode;
    function namedItem(name : DOMString) : IDOMNode;
  end;

  TjsHTMLOptionsCollection = class(TjsDOMObject,IHTMLOptionsCollection)
  protected
    function item(index : LongInt) : IDOMNode;
    function namedItem(name : DOMString) : IDOMNode;
  end;

  TjsHTMLDocument = class(TjsDOMDocument,IHTMLDocument)
  protected
    function Get_anchors : IHTMLCollection; //property anchors : IHTMLCollection read Get_anchors;
    function Get_applets : IHTMLCollection; //property applets : IHTMLCollection read Get_applets;
    function Get_body : IHTMLElement;//
    procedure Set_body(AValue : IHTMLElement);
    function Get_cookie : DOMString;//
    procedure Set_cookie(AValue : DOMString);
    function Get_domain : DOMString; //property domain : DOMString read Get_domain;
    function Get_forms : IHTMLCollection; //property forms : IHTMLCollection read Get_forms;
    function Get_images : IHTMLCollection; //property images : IHTMLCollection read Get_images;
    function Get_links : IHTMLCollection; //property links : IHTMLCollection read Get_links;
    function Get_referrer : DOMString; //property referrer : DOMString read Get_referrer;
    function Get_title : DOMString;//
    procedure Set_title(AValue : DOMString);
    function Get_URL : DOMString; //property URL : DOMString read Get_URL;

    procedure open();
    procedure close();
    procedure write(text : DOMString);
    procedure writeln(text : DOMString);
    function getElementsByName(elementName : DOMString) : IDOMNodeList;
  end;

  TjsHTMLHtmlElement = class(TjsHTMLElement,IHTMLHtmlElement)
  protected
    function Get_version : DOMString;
    procedure Set_version(AValue : DOMString);
  end;

  TjsHTMLHeadElement = class(TjsHTMLElement,IHTMLHeadElement)
  protected
    function Get_profile : DOMString;
    procedure Set_profile(AValue : DOMString);
  end;

  TjsHTMLLinkElement = class(TjsHTMLElement,IHTMLLinkElement)
  protected
    function Get_charset : DOMString;
    procedure Set_charset(AValue : DOMString);
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    function Get_href : DOMString;
    procedure Set_href(AValue : DOMString);
    function Get_hreflang : DOMString;
    procedure Set_hreflang(AValue : DOMString);
    function Get_media : DOMString;
    procedure Set_media(AValue : DOMString);
    function Get_rel : DOMString;
    procedure Set_rel(AValue : DOMString);
    function Get_rev : DOMString;
    procedure Set_rev(AValue : DOMString);
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
  end;

  TjsHTMLTitleElement = class(TjsHTMLElement,IHTMLTitleElement)
  protected
    function Get_text : DOMString;
    procedure Set_text(AValue : DOMString);
  end;

  TjsHTMLMetaElement = class(TjsHTMLElement,IHTMLMetaElement)
  protected
    function Get_content : DOMString;
    procedure Set_content(AValue : DOMString);
    function Get_httpEquiv : DOMString;
    procedure Set_httpEquiv(AValue : DOMString);
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    function Get_scheme : DOMString;
    procedure Set_scheme(AValue : DOMString);
  end;

  TjsHTMLBaseElement = class(TjsHTMLElement,IHTMLBaseElement)
  protected
    function Get_href : DOMString;
    procedure Set_href(AValue : DOMString);
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
  end;

  TjsHTMLFormElement = class(TjsHTMLElement,IHTMLFormElement)
  protected
    function Get_acceptCharset : DOMString;
    procedure Set_acceptCharset(AValue : DOMString);
    function Get_action : DOMString;
    procedure Set_action(AValue : DOMString);
    function Get_elements : IHTMLCollection;
    function Get_enctype : DOMString;
    procedure Set_enctype(AValue : DOMString);
    function Get_form : IHTMLFormElement;
    function Get_length : LongInt;
    function Get_method : DOMString;
    procedure Set_method(AValue : DOMString);
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    function Get_prompt : DOMString;
    procedure Set_prompt(AValue : DOMString);
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);

    procedure submit();
    procedure reset();
  end;

  TjsHTMLStyleElement = class(TjsHTMLElement,IHTMLStyleElement)
  protected
    function Get_disabled : Boolean;
    function Get_media : DOMString;
    function Get_type : DOMString;
    procedure Set_disabled(AValue : Boolean);
    procedure Set_media(AValue : DOMString);
    procedure Set_type(AValue : DOMString);
  end;

  TjsHTMLBodyElement = class(TjsHTMLElement,IHTMLBodyElement)
  protected
    function Get_aLink : DOMString;// procedure Set_aLink(AValue : DOMString);
    function Get_background : DOMString;// procedure Set_background(AValue : DOMString);
    function Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
    function Get_link : DOMString;// procedure Set_link(AValue : DOMString);
    function Get_text : DOMString;// procedure Set_text(AValue : DOMString);
    function Get_vLink : DOMString;// procedure Set_vLink(AValue : DOMString);
    procedure Set_aLink(AValue : DOMString); //property aLink : DOMString read Get_aLink write Set_aLink;
    procedure Set_background(AValue : DOMString); //property background : DOMString read Get_background write Set_background;
    procedure Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
    procedure Set_link(AValue : DOMString); //property link : DOMString read Get_link write Set_link;
    procedure Set_text(AValue : DOMString); //property text : DOMString read Get_text write Set_text;
    procedure Set_vLink(AValue : DOMString); //property vLink : DOMString read Get_vLink write Set_vLink;
  end;

  TjsHTMLSelectElement = class(TjsHTMLElement,IHTMLSelectElement)
  protected
    function Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_multiple : Boolean;// procedure Set_multiple(AValue : Boolean);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_options : IHTMLOptionsCollection; //property options : IHTMLOptionsCollection read Get_options;
    function Get_selectedIndex : LongInt;// procedure Set_selectedIndex(AValue : LongInt);
    function Get_size : LongInt;// procedure Set_size(AValue : LongInt);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_type : DOMString; //property _type : DOMString read Get_type;
    function Get_value : DOMString;// procedure Set_value(AValue : DOMString);
    procedure add(element : IHTMLElement; before : IHTMLElement);// procedure remove(index : LongInt);
    procedure Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
    procedure Set_multiple(AValue : Boolean); //property multiple : Boolean read Get_multiple write Set_multiple;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_selectedIndex(AValue : LongInt); //property selectedIndex : LongInt read Get_selectedIndex write Set_selectedIndex;
    procedure Set_size(AValue : LongInt); //property size : LongInt read Get_size write Set_size;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;

    procedure remove(index : LongInt);
    procedure blur();
    procedure focus();
  end;

  TjsHTMLOptGroupElement = class(TjsHTMLElement,IHTMLOptGroupElement)
  protected
    function Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
    function Get_label : DOMString;// procedure Set_label(AValue : DOMString);
    procedure Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
    procedure Set_label(AValue : DOMString); //property _label : DOMString read Get_label write Set_label;
  end;

  TjsHTMLOptionElement = class(TjsHTMLElement,IHTMLOptionElement)
  protected
    function Get_defaultSelected : Boolean;// procedure Set_defaultSelected(AValue : Boolean);
    function Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_index : LongInt; //property _index : longInt read Get_index;
    function Get_label : DOMString;// procedure Set_label(AValue : DOMString);
    function Get_selected : Boolean;// procedure Set_selected(AValue : Boolean);
    function Get_text : DOMString; //property text : DOMString read Get_text;
    function Get_value : DOMString;// procedure Set_value(AValue : DOMString);
    procedure Set_defaultSelected(AValue : Boolean); //property defaultSelected : Boolean read Get_defaultSelected write Set_defaultSelected;
    procedure Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
    procedure Set_label(AValue : DOMString); //property _label : DOMString read Get_label write Set_label;
    procedure Set_selected(AValue : Boolean); //property selected : Boolean read Get_selected write Set_selected;
    procedure Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
  end;

  TjsHTMLInputElement = class(TjsHTMLElement,IHTMLInputElement)
  protected
    function Get_accept : DOMString;// procedure Set_accept(AValue : DOMString);
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
    function Get_checked : Boolean;// procedure Set_checked(AValue : Boolean);
    function Get_defaultChecked : Boolean;// procedure Set_defaultChecked(AValue : Boolean);
    function Get_defaultValue : DOMString;// procedure Set_defaultValue(AValue : DOMString);
    function Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_maxLength : LongInt;// procedure Set_maxLength(AValue : LongInt);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_readOnly : Boolean;// procedure Set_readOnly(AValue : Boolean);
    function Get_src : DOMString;// procedure Set_src(AValue : DOMString);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    function Get_useMap : DOMString;// procedure Set_useMap(AValue : DOMString);
    function Get_value : DOMString;// procedure Set_value(AValue : DOMString);
    procedure Set_accept(AValue : DOMString); //property accept : DOMString read Get_accept write Set_accept;
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
    procedure Set_checked(AValue : Boolean); //property checked : Boolean read Get_checked write Set_checked;
    procedure Set_defaultChecked(AValue : Boolean); //property defaultChecked : Boolean read Get_defaultChecked write Set_defaultChecked;
    procedure Set_defaultValue(AValue : DOMString); //property defaultValue : DOMString read Get_defaultValue write Set_defaultValue;
    procedure Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
    procedure Set_maxLength(AValue : LongInt); //property maxLength : LongInt read Get_maxLength write Set_maxLength;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_readOnly(AValue : Boolean); //property _readOnly : Boolean read Get_readOnly write Set_readOnly;
    procedure Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
    procedure Set_useMap(AValue : DOMString); //property useMap : DOMString read Get_useMap write Set_useMap;
    procedure Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;

    procedure blur();
    procedure focus();
    procedure select();
    procedure click();
  end;

  TjsHTMLTextAreaElement = class(TjsHTMLElement,IHTMLTextAreaElement)
  protected
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_cols : LongInt;// procedure Set_cols(AValue : LongInt);
    function Get_defaultValue : DOMString;// procedure Set_defaultValue(AValue : DOMString);
    function Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_readOnly : Boolean;// procedure Set_readOnly(AValue : Boolean);
    function Get_rows : LongInt;// procedure Set_rows(AValue : LongInt);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_type : DOMString; //property _type : DOMString read Get_type;
    function Get_value : DOMString;// procedure Set_value(AValue : DOMString);
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_cols(AValue : LongInt); //property cols : LongInt read Get_cols write Set_cols;
    procedure Set_defaultValue(AValue : DOMString); //property defaultValue : DOMString read Get_defaultValue write Set_defaultValue;
    procedure Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_readOnly(AValue : Boolean); //property readOnly : Boolean read Get_readOnly write Set_readOnly;
    procedure Set_rows(AValue : LongInt); //property rows : LongInt read Get_rows write Set_rows;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;

    procedure blur();
    procedure focus();
    procedure select();
  end;

  TjsHTMLButtonElement = class(TjsHTMLElement,IHTMLButtonElement)
  protected
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_type : DOMString; //property _type : DOMString read Get_type;
    function Get_value : DOMString;// procedure Set_value(AValue : DOMString);
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
  end;

  TjsHTMLLabelElement = class(TjsHTMLElement,IHTMLLabelElement)
  protected
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_htmlFor : DOMString;// procedure Set_htmlFor(AValue : DOMString);
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_htmlFor(AValue : DOMString); //property htmlFor : DOMString read Get_htmlFor write Set_htmlFor;
  end;

  TjsHTMLFieldSetElement = class(TjsHTMLElement,IHTMLFieldSetElement)
  protected
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
  end;

  TjsHTMLLegendElement = class(TjsHTMLElement,IHTMLLegendElement)
  protected
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
  end;

  TjsHTMLUListElement = class(TjsHTMLElement,IHTMLUListElement)
  protected
    function Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    procedure Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
  end;

  TjsHTMLOListElement = class(TjsHTMLElement,IHTMLOListElement)
  protected
    function Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
    function Get_start : LongInt;// procedure Set_start(AValue : LongInt);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    procedure Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
    procedure Set_start(AValue : LongInt); //property start : LongInt read Get_start write Set_start;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
  end;

  TjsHTMLDListElement = class(TjsHTMLElement,IHTMLDListElement)
  protected
    function Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
    procedure Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
  end;

  TjsHTMLDirectoryElement = class(TjsHTMLElement,IHTMLDirectoryElement)
  protected
    function Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
    procedure Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
  end;

  TjsHTMLMenuElement = class(TjsHTMLElement,IHTMLMenuElement)
  protected
    function Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
    procedure Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
  end;

  TjsHTMLLIElement = class(TjsHTMLElement,IHTMLLIElement)
  protected
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    function Get_value : LongInt;// procedure Set_value(AValue : LongInt);
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
    procedure Set_value(AValue : LongInt); //property value : LongInt read Get_value write Set_value;
  end;

  TjsHTMLDivElement = class(TjsHTMLElement,IHTMLDivElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
  end;

  TjsHTMLParagraphElement = class(TjsHTMLElement,IHTMLParagraphElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
  end;

  TjsHTMLHeadingElement = class(TjsHTMLElement,IHTMLHeadingElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
  end;

  TjsHTMLQuoteElement = class(TjsHTMLElement,IHTMLQuoteElement)
  protected
    function Get_cite : DOMString;// procedure Set_cite(AValue : DOMString);
    procedure Set_cite(AValue : DOMString); //property cite : DOMString read Get_cite write Set_cite;
  end;

  TjsHTMLPreElement = class(TjsHTMLElement,IHTMLPreElement)
  protected
    function Get_width : LongInt;// procedure Set_width(AValue : LongInt);
    procedure Set_width(AValue : LongInt); //property width : LongInt read Get_width write Set_width;
  end;

  TjsHTMLBRElement = class(TjsHTMLElement,IHTMLBRElement)
  protected
    function Get_clear : DOMString;// procedure Set_clear(AValue : DOMString);
    procedure Set_clear(AValue : DOMString); //property clear : DOMString read Get_clear write Set_clear;
  end;

  TjsHTMLBaseFontElement = class(TjsHTMLElement,IHTMLBaseFontElement)
  protected
    function Get_color : DOMString;// procedure Set_color(AValue : DOMString);
    function Get_face : DOMString;// procedure Set_face(AValue : DOMString);
    function Get_size : LongInt;// procedure Set_size(AValue : LongInt);
    procedure Set_color(AValue : DOMString); //property color : DOMString read Get_color write Set_color;
    procedure Set_face(AValue : DOMString); //property face : DOMString read Get_face write Set_face;
    procedure Set_size(AValue : LongInt); //property size : LongInt read Get_size write Set_size;
  end;

  TjsHTMLFontElement = class(TjsHTMLElement,IHTMLFontElement)
  protected
    function Get_color : DOMString;// procedure Set_color(AValue : DOMString);
    function Get_face : DOMString;// procedure Set_face(AValue : DOMString);
    function Get_size : DOMString;// procedure Set_size(AValue : DOMString);
    procedure Set_color(AValue : DOMString); //property color : DOMString read Get_color write Set_color;
    procedure Set_face(AValue : DOMString); //property face : DOMString read Get_face write Set_face;
    procedure Set_size(AValue : DOMString); //property size : DOMString read Get_size write Set_size;
  end;

  TjsHTMLHRElement = class(TjsHTMLElement,IHTMLHRElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_noShade : Boolean;// procedure Set_noShade(AValue : Boolean);
    function Get_size : DOMString;// procedure Set_size(AValue : DOMString);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_noShade(AValue : Boolean); //property noShade : Boolean read Get_noShade write Set_noShade;
    procedure Set_size(AValue : DOMString); //property size : DOMString read Get_size write Set_size;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

  TjsHTMLModElement = class(TjsHTMLElement,IHTMLModElement)
  protected
    function Get_cite : DOMString;// procedure Set_cite(AValue : DOMString);
    function Get_dateTime : DOMString;// procedure Set_dateTime(AValue : DOMString);
    procedure Set_cite(AValue : DOMString); //property cite : DOMString read Get_cite write Set_cite;
    procedure Set_dateTime(AValue : DOMString); //property dateTime : DOMString read Get_dateTime write Set_dateTime;
  end;

  TjsHTMLAnchorElement = class(TjsHTMLElement,IHTMLAnchorElement)
  protected
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_charset : DOMString;// procedure Set_charset(AValue : DOMString);
    function Get_coords : DOMString;// procedure Set_coords(AValue : DOMString);
    function Get_href : DOMString;// procedure Set_href(AValue : DOMString);
    function Get_hreflang : DOMString;// procedure Set_hreflang(AValue : DOMString);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_rel : DOMString;// procedure Set_rel(AValue : DOMString);
    function Get_rev : DOMString;// procedure Set_rev(AValue : DOMString);
    function Get_shape : DOMString;// procedure Set_shape(AValue : DOMString);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_target : DOMString;// procedure Set_target(AValue : DOMString);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_charset(AValue : DOMString); //property charset : DOMString read Get_charset write Set_charset;
    procedure Set_coords(AValue : DOMString); //property coords : DOMString read Get_coords write Set_coords;
    procedure Set_href(AValue : DOMString); //property href : DOMString read Get_href write Set_href;
    procedure Set_hreflang(AValue : DOMString); //property hreflang : DOMString read Get_hreflang write Set_hreflang;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_rel(AValue : DOMString); //property rel : DOMString read Get_rel write Set_rel;
    procedure Set_rev(AValue : DOMString); //property rev : DOMString read Get_rev write Set_rev;
    procedure Set_shape(AValue : DOMString); //property shape : DOMString read Get_shape write Set_shape;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;

    procedure blur();
    procedure focus();
  end;

  TjsHTMLImageElement = class(TjsHTMLElement,IHTMLImageElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
    function Get_border : DOMString;// procedure Set_border(AValue : DOMString);
    function Get_height : LongInt;// procedure Set_height(AValue : LongInt);
    function Get_hspace : LongInt;// procedure Set_hspace(AValue : LongInt);
    function Get_isMap : Boolean;// procedure Set_isMap(AValue : Boolean);
    function Get_longDesc : DOMString;// procedure Set_longDesc(AValue : DOMString);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_src : DOMString;// procedure Set_src(AValue : DOMString);
    function Get_useMap : DOMString;// procedure Set_useMap(AValue : DOMString);
    function Get_vspace : LongInt;// procedure Set_vspace(AValue : LongInt);
    function Get_width : LongInt;// procedure Set_width(AValue : LongInt);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
    procedure Set_border(AValue : DOMString); //property border : DOMString read Get_border write Set_border;
    procedure Set_height(AValue : LongInt); //property height : LongInt read Get_height write Set_height;
    procedure Set_hspace(AValue : LongInt); //property hspace : LongInt read Get_hspace write Set_hspace;
    procedure Set_isMap(AValue : Boolean); //property isMap : Boolean read Get_isMap write Set_isMap;
    procedure Set_longDesc(AValue : DOMString); //property longDesc : DOMString read Get_longDesc write Set_longDesc;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
    procedure Set_useMap(AValue : DOMString); //property useMap : DOMString read Get_useMap write Set_useMap;
    procedure Set_vspace(AValue : LongInt); //property vspace : LongInt read Get_vspace write Set_vspace;
    procedure Set_width(AValue : LongInt); //property width : LongInt read Get_width write Set_width;
  end;

  TjsHTMLObjectElement = class(TjsHTMLElement,IHTMLObjectElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_archive : DOMString;// procedure Set_archive(AValue : DOMString);
    function Get_border : DOMString;// procedure Set_border(AValue : DOMString);
    function Get_code : DOMString;// procedure Set_code(AValue : DOMString);
    function Get_codeBase : DOMString;// procedure Set_codeBase(AValue : DOMString);
    function Get_codeType : DOMString;// procedure Set_codeType(AValue : DOMString);
    function Get_contentDocument : IHTMLDocument; //property contentDocument : IHTMLDocument read Get_contentDocument;
    function Get_data : DOMString;// procedure Set_data(AValue : DOMString);
    function Get_declare : Boolean;// procedure Set_declare(AValue : Boolean);
    function Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
    function Get_height : DOMString;// procedure Set_height(AValue : DOMString);
    function Get_hspace : LongInt;// procedure Set_hspace(AValue : LongInt);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_standby : DOMString;// procedure Set_standby(AValue : DOMString);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    function Get_useMap : DOMString;// procedure Set_useMap(AValue : DOMString);
    function Get_vspace : LongInt;// procedure Set_vspace(AValue : LongInt);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_archive(AValue : DOMString); //property archive : DOMString read Get_archive write Set_archive;
    procedure Set_border(AValue : DOMString); //property border : DOMString read Get_border write Set_border;
    procedure Set_code(AValue : DOMString); //property code : DOMString read Get_code write Set_code;
    procedure Set_codeBase(AValue : DOMString); //property codeBase : DOMString read Get_codeBase write Set_codeBase;
    procedure Set_codeType(AValue : DOMString); //property codeType : DOMString read Get_codeType write Set_codeType;
    procedure Set_data(AValue : DOMString); //property data : DOMString read Get_data write Set_data;
    procedure Set_declare(AValue : Boolean); //property declare : Boolean read Get_declare write Set_declare;
    procedure Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
    procedure Set_hspace(AValue : LongInt); //property hspace : LongInt read Get_hspace write Set_hspace;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_standby(AValue : DOMString); //property standby : DOMString read Get_standby write Set_standby;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
    procedure Set_useMap(AValue : DOMString); //property useMap : DOMString read Get_useMap write Set_useMap;
    procedure Set_vspace(AValue : LongInt); //property vspace : LongInt read Get_vspace write Set_vspace;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

  TjsHTMLParamElement = class(TjsHTMLElement,IHTMLParamElement)
  protected
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    function Get_value : DOMString;// procedure Set_value(AValue : DOMString);
    function Get_valueType : DOMString;// procedure Set_valueType(AValue : DOMString);
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
    procedure Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
    procedure Set_valueType(AValue : DOMString); //property valueType : DOMString read Get_valueType write Set_valueType;
  end;

  TjsHTMLAppletElement = class(TjsHTMLElement,IHTMLAppletElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
    function Get_archive : DOMString;// procedure Set_archive(AValue : DOMString);
    function Get_code : DOMString;// procedure Set_code(AValue : DOMString);
    function Get_codeBase : DOMString;// procedure Set_codeBase(AValue : DOMString);
    function Get_height : DOMString;// procedure Set_height(AValue : DOMString);
    function Get_hspace : LongInt;// procedure Set_hspace(AValue : LongInt);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_object : DOMString;// procedure Set_object(AValue : DOMString);
    function Get_vspace : LongInt;// procedure Set_vspace(AValue : LongInt);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
    procedure Set_archive(AValue : DOMString); //property archive : DOMString read Get_archive write Set_archive;
    procedure Set_code(AValue : DOMString); //property code : DOMString read Get_code write Set_code;
    procedure Set_codeBase(AValue : DOMString); //property codeBase : DOMString read Get_codeBase write Set_codeBase;
    procedure Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
    procedure Set_hspace(AValue : LongInt); //property hspace : LongInt read Get_hspace write Set_hspace;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_object(AValue : DOMString); //property _object : DOMString read Get_object write Set_object;
    procedure Set_vspace(AValue : LongInt); //property vspace : LongInt read Get_vspace write Set_vspace;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

  TjsHTMLMapElement = class(TjsHTMLElement,IHTMLMapElement)
  protected
    function Get_areas : IHTMLCollection; //property areas : IHTMLCollection read Get_areas;
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
  end;

  TjsHTMLAreaElement = class(TjsHTMLElement,IHTMLAreaElement)
  protected
    function Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
    function Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
    function Get_coords : DOMString;// procedure Set_coords(AValue : DOMString);
    function Get_href : DOMString;// procedure Set_href(AValue : DOMString);
    function Get_noHref : Boolean;// procedure Set_noHref(AValue : Boolean);
    function Get_shape : DOMString;// procedure Set_shape(AValue : DOMString);
    function Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
    function Get_target : DOMString;// procedure Set_target(AValue : DOMString);
    procedure Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
    procedure Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
    procedure Set_coords(AValue : DOMString); //property coords : DOMString read Get_coords write Set_coords;
    procedure Set_href(AValue : DOMString); //property href : DOMString read Get_href write Set_href;
    procedure Set_noHref(AValue : Boolean); //property noHref : Boolean read Get_noHref write Set_noHref;
    procedure Set_shape(AValue : DOMString); //property shape : DOMString read Get_shape write Set_shape;
    procedure Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
  end;

  TjsHTMLScriptElement = class(TjsHTMLElement,IHTMLScriptElement)
  protected
    function Get_charset : DOMString;// procedure Set_charset(AValue : DOMString);
    function Get_defer : Boolean;// procedure Set_defer(AValue : Boolean);
    function Get_event : DOMString;// procedure Set_event(AValue : DOMString);
    function Get_htmlFor : DOMString;// procedure Set_htmlFor(AValue : DOMString);
    function Get_src : DOMString;// procedure Set_src(AValue : DOMString);
    function Get_text : DOMString;// procedure Set_text(AValue : DOMString);
    function Get_type : DOMString;// procedure Set_type(AValue : DOMString);
    procedure Set_charset(AValue : DOMString); //property charset : DOMString read Get_charset write Set_charset;
    procedure Set_defer(AValue : Boolean); //property defer : Boolean read Get_defer write Set_defer;
    procedure Set_event(AValue : DOMString); //property event : DOMString read Get_event write Set_event;
    procedure Set_htmlFor(AValue : DOMString); //property htmlFor : DOMString read Get_htmlFor write Set_htmlFor;
    procedure Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
    procedure Set_text(AValue : DOMString); //property text : DOMString read Get_text write Set_text;
    procedure Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
  end;

  TjsHTMLTableCaptionElement = class(TjsHTMLElement, IHTMLTableCaptionElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
    function Get_border : DOMString;// procedure Set_border(AValue : DOMString);
    function Get_caption : IHTMLTableCaptionElement;// procedure Set_caption(AValue : IHTMLTableCaptionElement);
    function Get_cellPadding : DOMString;// procedure Set_cellPadding(AValue : DOMString);
    function Get_cellSpacing : DOMString;// procedure Set_cellSpacing(AValue : DOMString);
    function Get_frame : DOMString;// procedure Set_frame(AValue : DOMString);
    function Get_rows : IHTMLCollection; //property rows : IHTMLCollection read Get_rows;
    function Get_rules : DOMString;// procedure Set_rules(AValue : DOMString);
    function Get_summary : DOMString;// procedure Set_summary(AValue : DOMString);
    function Get_tBodies : IHTMLCollection; //property tBodies : IHTMLCollection read Get_tBodies;
    function Get_tFoot : IHTMLTableSectionElement;// procedure Set_tFoot(AValue : IHTMLTableSectionElement);
    function Get_tHead : IHTMLTableSectionElement;// procedure Set_tHead(AValue : IHTMLTableSectionElement);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    function insertRow(index : LongInt) : IHTMLElement;// procedure deleteRow(index : LongInt);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
    procedure Set_border(AValue : DOMString); //property border : DOMString read Get_border write Set_border;
    procedure Set_caption(AValue : IHTMLTableCaptionElement); //property caption : IHTMLTableCaptionElement read Get_caption write Set_caption;
    procedure Set_cellPadding(AValue : DOMString); //property cellPadding : DOMString read Get_cellPadding write Set_cellPadding;
    procedure Set_cellSpacing(AValue : DOMString); //property cellSpacing : DOMString read Get_cellSpacing write Set_cellSpacing;
    procedure Set_frame(AValue : DOMString); //property frame : DOMString read Get_frame write Set_frame;
    procedure Set_rules(AValue : DOMString); //property rules : DOMString read Get_rules write Set_rules;
    procedure Set_summary(AValue : DOMString); //property summary : DOMString read Get_summary write Set_summary;
    procedure Set_tFoot(AValue : IHTMLTableSectionElement); //property tFoot : IHTMLTableSectionElement read Get_tFoot write Set_tFoot;
    procedure Set_tHead(AValue : IHTMLTableSectionElement); //property tHead : IHTMLTableSectionElement read Get_tHead write Set_tHead;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

  TjsHTMLTableColElement = class(TjsHTMLElement,IHTMLTableColElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
    function Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
    function Get_span : LongInt;// procedure Set_span(AValue : LongInt);
    function Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
    procedure Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
    procedure Set_span(AValue : LongInt); //property span : LongInt read Get_span write Set_span;
    procedure Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

  TjsHTMLTableSectionElement = class(TjsHTMLElement,IHTMLTableSectionElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
    function Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
    function Get_rows : IHTMLCollection; //property rows : IHTMLCollection read Get_rows;
    function Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
    function insertRow(index : LongInt) : IHTMLElement;// procedure deleteRow(index : LongInt);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
    procedure Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
    procedure Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;

    procedure deleteRow(index : LongInt);
  end;

  TjsHTMLTableRowElement = class(TjsHTMLElement,IHTMLTableRowElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
    function Get_cells : IHTMLCollection; //property cells : IHTMLCollection read Get_cells;
    function Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
    function Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
    function Get_rowIndex : LongInt; //property rowIndex : LongInt read Get_rowIndex;
    function Get_sectionRowIndex : LongInt; //property sectionRowIndex : LongInt read Get_sectionRowIndex;
    function Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
    function insertCell(index : LongInt) : IHTMLElement;// procedure deleteCell(index : LongInt);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
    procedure Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
    procedure Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
    procedure Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
    procedure deleteCell(index : LongInt);
  end;

  TjsHTMLTableCellElement = class(TjsHTMLElement,IHTMLTableCellElement)
  protected
    function Get_abbr : DOMString;// procedure Set_abbr(AValue : DOMString);
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_axis : DOMString;// procedure Set_axis(AValue : DOMString);
    function Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
    function Get_cellIndex : LongInt; //property cellIndex : LongInt read Get_cellIndex;
    function Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
    function Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
    function Get_colSpan : LongInt;// procedure Set_colSpan(AValue : LongInt);
    function Get_headers : DOMString;// procedure Set_headers(AValue : DOMString);
    function Get_height : DOMString;// procedure Set_height(AValue : DOMString);
    function Get_noWrap : Boolean;// procedure Set_noWrap(AValue : Boolean);
    function Get_rowSpan : LongInt;// procedure Set_rowSpan(AValue : LongInt);
    function Get_scope : DOMString;// procedure Set_scope(AValue : DOMString);
    function Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    procedure Set_abbr(AValue : DOMString); //property abbr : DOMString read Get_abbr write Set_abbr;
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_axis(AValue : DOMString); //property axis : DOMString read Get_axis write Set_axis;
    procedure Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
    procedure Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
    procedure Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
    procedure Set_colSpan(AValue : LongInt); //property colSpan : LongInt read Get_colSpan write Set_colSpan;
    procedure Set_headers(AValue : DOMString); //property headers : DOMString read Get_headers write Set_headers;
    procedure Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
    procedure Set_noWrap(AValue : Boolean); //property noWrap : Boolean read Get_noWrap write Set_noWrap;
    procedure Set_rowSpan(AValue : LongInt); //property rowSpan : LongInt read Get_rowSpan write Set_rowSpan;
    procedure Set_scope(AValue : DOMString); //property scope : DOMString read Get_scope write Set_scope;
    procedure Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

  TjsHTMLFrameSetElement = class(TjsHTMLElement,IHTMLFrameSetElement)
  protected
    function Get_cols : DOMString;// procedure Set_cols(AValue : DOMString);
    function Get_rows : DOMString;// procedure Set_rows(AValue : DOMString);
    procedure Set_cols(AValue : DOMString); //property cols : DOMString read Get_cols write Set_cols;
    procedure Set_rows(AValue : DOMString); //property rows : DOMString read Get_rows write Set_rows;
  end;

  TjsHTMLFrameElement = class(TjsHTMLElement,IHTMLFrameElement)
  protected
    function Get_contentDocument : IHTMLDocument; //property contentDocument : IHTMLDocument read Get_contentDocument;
    function Get_frameBorder : DOMString;// procedure Set_frameBorder(AValue : DOMString);
    function Get_longDesc : DOMString;// procedure Set_longDesc(AValue : DOMString);
    function Get_marginHeight : DOMString;// procedure Set_marginHeight(AValue : DOMString);
    function Get_marginWidth : DOMString;// procedure Set_marginWidth(AValue : DOMString);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_noResize : Boolean;// procedure Set_noResize(AValue : Boolean);
    function Get_scrolling : DOMString;// procedure Set_scrolling(AValue : DOMString);
    function Get_src : DOMString;// procedure Set_src(AValue : DOMString);
    procedure Set_frameBorder(AValue : DOMString); //property frameBorder : DOMString read Get_frameBorder write Set_frameBorder;
    procedure Set_longDesc(AValue : DOMString); //property longDesc : DOMString read Get_longDesc write Set_longDesc;
    procedure Set_marginHeight(AValue : DOMString); //property marginHeight : DOMString read Get_marginHeight write Set_marginHeight;
    procedure Set_marginWidth(AValue : DOMString); //property marginWidth : DOMString read Get_marginWidth write Set_marginWidth;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_noResize(AValue : Boolean); //property noResize : Boolean read Get_noResize write Set_noResize;
    procedure Set_scrolling(AValue : DOMString); //property scrolling : DOMString read Get_scrolling write Set_scrolling;
    procedure Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
  end;

  TjsHTMLIFrameElement = class(TjsHTMLElement,IHTMLIFrameElement)
  protected
    function Get_align : DOMString;// procedure Set_align(AValue : DOMString);
    function Get_contentDocument : IHTMLDocument; //property contentDocument : IHTMLDocument read Get_contentDocument;
    function Get_frameBorder : DOMString;// procedure Set_frameBorder(AValue : DOMString);
    function Get_height : DOMString;// procedure Set_height(AValue : DOMString);
    function Get_longDesc : DOMString;// procedure Set_longDesc(AValue : DOMString);
    function Get_marginHeight : DOMString;// procedure Set_marginHeight(AValue : DOMString);
    function Get_marginWidth : DOMString;// procedure Set_marginWidth(AValue : DOMString);
    function Get_name : DOMString;// procedure Set_name(AValue : DOMString);
    function Get_scrolling : DOMString;// procedure Set_scrolling(AValue : DOMString);
    function Get_src : DOMString;// procedure Set_src(AValue : DOMString);
    function Get_width : DOMString;// procedure Set_width(AValue : DOMString);
    procedure Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
    procedure Set_frameBorder(AValue : DOMString); //property frameBorder : DOMString read Get_frameBorder write Set_frameBorder;
    procedure Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
    procedure Set_longDesc(AValue : DOMString); //property longDesc : DOMString read Get_longDesc write Set_longDesc;
    procedure Set_marginHeight(AValue : DOMString); //property marginHeight : DOMString read Get_marginHeight write Set_marginHeight;
    procedure Set_marginWidth(AValue : DOMString); //property marginWidth : DOMString read Get_marginWidth write Set_marginWidth;
    procedure Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
    procedure Set_scrolling(AValue : DOMString); //property scrolling : DOMString read Get_scrolling write Set_scrolling;
    procedure Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
    procedure Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
  end;

implementation

{ TjsHTMLIFrameElement }

function TjsHTMLIFrameElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLIFrameElement.Get_contentDocument : IHTMLDocument; //property contentDocument : IHTMLDocument read Get_contentDocument;
begin
  GetPropertyValueIntf('contentDocument', Result, TjsHTMLDocument);
end;

function TjsHTMLIFrameElement.Get_frameBorder : DOMString;// procedure Set_frameBorder(AValue : DOMString);
begin
  GetpropertyValue('frameBorder', Result)
end;

function TjsHTMLIFrameElement.Get_height : DOMString;// procedure Set_height(AValue : DOMString);
begin
  GetpropertyValue('height', Result)
end;

function TjsHTMLIFrameElement.Get_longDesc : DOMString;// procedure Set_longDesc(AValue : DOMString);
begin
  GetpropertyValue('longDesc', Result)
end;

function TjsHTMLIFrameElement.Get_marginHeight : DOMString;// procedure Set_marginHeight(AValue : DOMString);
begin
  GetpropertyValue('marginHeight', Result)
end;

function TjsHTMLIFrameElement.Get_marginWidth : DOMString;// procedure Set_marginWidth(AValue : DOMString);
begin
  GetpropertyValue('marginWidth', Result)
end;

function TjsHTMLIFrameElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLIFrameElement.Get_scrolling : DOMString;// procedure Set_scrolling(AValue : DOMString);
begin
  GetpropertyValue('scrolling', Result)
end;

function TjsHTMLIFrameElement.Get_src : DOMString;// procedure Set_src(AValue : DOMString);
begin
  GetpropertyValue('src', Result)
end;

function TjsHTMLIFrameElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLIFrameElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLIFrameElement.Set_frameBorder(AValue : DOMString); //property frameBorder : DOMString read Get_frameBorder write Set_frameBorder;
begin
  SetPropertyValue('frameBorder', AValue);
end;

procedure TjsHTMLIFrameElement.Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
begin
  SetPropertyValue('height', AValue);
end;

procedure TjsHTMLIFrameElement.Set_longDesc(AValue : DOMString); //property longDesc : DOMString read Get_longDesc write Set_longDesc;
begin
  SetPropertyValue('longDesc', AValue);
end;

procedure TjsHTMLIFrameElement.Set_marginHeight(AValue : DOMString); //property marginHeight : DOMString read Get_marginHeight write Set_marginHeight;
begin
  SetPropertyValue('marginHeight', AValue);
end;

procedure TjsHTMLIFrameElement.Set_marginWidth(AValue : DOMString); //property marginWidth : DOMString read Get_marginWidth write Set_marginWidth;
begin
  SetPropertyValue('marginWidth', AValue);
end;

procedure TjsHTMLIFrameElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLIFrameElement.Set_scrolling(AValue : DOMString); //property scrolling : DOMString read Get_scrolling write Set_scrolling;
begin
  SetPropertyValue('scrolling', AValue);
end;

procedure TjsHTMLIFrameElement.Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
begin
  SetPropertyValue('src', AValue);
end;

procedure TjsHTMLIFrameElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLFrameElement }

function TjsHTMLFrameElement.Get_contentDocument : IHTMLDocument; //property contentDocument : IHTMLDocument read Get_contentDocument;
begin
  GetPropertyValueIntf('contentDocument', Result, TjsHTMLDocument)
end;

function TjsHTMLFrameElement.Get_frameBorder : DOMString;// procedure Set_frameBorder(AValue : DOMString);
begin
  GetpropertyValue('frameBorder', Result)
end;

function TjsHTMLFrameElement.Get_longDesc : DOMString;// procedure Set_longDesc(AValue : DOMString);
begin
  GetpropertyValue('longDesc', Result)
end;

function TjsHTMLFrameElement.Get_marginHeight : DOMString;// procedure Set_marginHeight(AValue : DOMString);
begin
  GetpropertyValue('marginHeight', Result)
end;

function TjsHTMLFrameElement.Get_marginWidth : DOMString;// procedure Set_marginWidth(AValue : DOMString);
begin
  GetpropertyValue('marginWidth', Result)
end;

function TjsHTMLFrameElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLFrameElement.Get_noResize : Boolean;// procedure Set_noResize(AValue : Boolean);
begin
  GetpropertyValue('noResize', Result)
end;

function TjsHTMLFrameElement.Get_scrolling : DOMString;// procedure Set_scrolling(AValue : DOMString);
begin
  GetpropertyValue('scrolling', Result)
end;

function TjsHTMLFrameElement.Get_src : DOMString;// procedure Set_src(AValue : DOMString);
begin
  GetpropertyValue('src', Result)
end;

procedure TjsHTMLFrameElement.Set_frameBorder(AValue : DOMString); //property frameBorder : DOMString read Get_frameBorder write Set_frameBorder;
begin
  SetPropertyValue('frameBorder', AValue);
end;

procedure TjsHTMLFrameElement.Set_longDesc(AValue : DOMString); //property longDesc : DOMString read Get_longDesc write Set_longDesc;
begin
  SetPropertyValue('longDesc', AValue);
end;

procedure TjsHTMLFrameElement.Set_marginHeight(AValue : DOMString); //property marginHeight : DOMString read Get_marginHeight write Set_marginHeight;
begin
  SetPropertyValue('marginHeight', AValue);
end;

procedure TjsHTMLFrameElement.Set_marginWidth(AValue : DOMString); //property marginWidth : DOMString read Get_marginWidth write Set_marginWidth;
begin
  SetPropertyValue('marginWidth', AValue);
end;

procedure TjsHTMLFrameElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLFrameElement.Set_noResize(AValue : Boolean); //property noResize : Boolean read Get_noResize write Set_noResize;
begin
  SetPropertyValue('noResize', AValue);
end;

procedure TjsHTMLFrameElement.Set_scrolling(AValue : DOMString); //property scrolling : DOMString read Get_scrolling write Set_scrolling;
begin
  SetPropertyValue('scrolling', AValue);
end;

procedure TjsHTMLFrameElement.Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
begin
  SetPropertyValue('src', AValue);
end;
{ TjsHTMLFrameSetElement }

function TjsHTMLFrameSetElement.Get_cols : DOMString;// procedure Set_cols(AValue : DOMString);
begin
  GetpropertyValue('cols', Result)
end;

function TjsHTMLFrameSetElement.Get_rows : DOMString;// procedure Set_rows(AValue : DOMString);
begin
  GetpropertyValue('rows', Result)
end;

procedure TjsHTMLFrameSetElement.Set_cols(AValue : DOMString); //property cols : DOMString read Get_cols write Set_cols;
begin
  SetPropertyValue('cols', AValue);
end;

procedure TjsHTMLFrameSetElement.Set_rows(AValue : DOMString); //property rows : DOMString read Get_rows write Set_rows;
begin
  SetPropertyValue('rows', AValue);
end;
{ TjsHTMLTableCellElement }

function TjsHTMLTableCellElement.Get_abbr : DOMString;// procedure Set_abbr(AValue : DOMString);
begin
  GetpropertyValue('abbr', Result)
end;

function TjsHTMLTableCellElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLTableCellElement.Get_axis : DOMString;// procedure Set_axis(AValue : DOMString);
begin
  GetpropertyValue('axis', Result)
end;

function TjsHTMLTableCellElement.Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
begin
  GetpropertyValue('bgColor', Result)
end;

function TjsHTMLTableCellElement.Get_cellIndex : LongInt; //property cellIndex : LongInt read Get_cellIndex;
begin
  GetpropertyValue('cellIndex', Result)
end;

function TjsHTMLTableCellElement.Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
begin
  GetpropertyValue('ch', Result)
end;

function TjsHTMLTableCellElement.Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
begin
  GetpropertyValue('chOff', Result)
end;

function TjsHTMLTableCellElement.Get_colSpan : LongInt;// procedure Set_colSpan(AValue : LongInt);
begin
  GetpropertyValue('colSpan', Result)
end;

function TjsHTMLTableCellElement.Get_headers : DOMString;// procedure Set_headers(AValue : DOMString);
begin
  GetpropertyValue('headers', Result)
end;

function TjsHTMLTableCellElement.Get_height : DOMString;// procedure Set_height(AValue : DOMString);
begin
  GetpropertyValue('height', Result)
end;

function TjsHTMLTableCellElement.Get_noWrap : Boolean;// procedure Set_noWrap(AValue : Boolean);
begin
  GetpropertyValue('noWrap', Result)
end;

function TjsHTMLTableCellElement.Get_rowSpan : LongInt;// procedure Set_rowSpan(AValue : LongInt);
begin
  GetpropertyValue('rowSpan', Result)
end;

function TjsHTMLTableCellElement.Get_scope : DOMString;// procedure Set_scope(AValue : DOMString);
begin
  GetpropertyValue('scope', Result)
end;

function TjsHTMLTableCellElement.Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
begin
  GetpropertyValue('vAlign', Result)
end;

function TjsHTMLTableCellElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLTableCellElement.Set_abbr(AValue : DOMString); //property abbr : DOMString read Get_abbr write Set_abbr;
begin
  SetPropertyValue('abbr', AValue);
end;

procedure TjsHTMLTableCellElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLTableCellElement.Set_axis(AValue : DOMString); //property axis : DOMString read Get_axis write Set_axis;
begin
  SetPropertyValue('axis', AValue);
end;

procedure TjsHTMLTableCellElement.Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
begin
  SetPropertyValue('bgColor', AValue);
end;

procedure TjsHTMLTableCellElement.Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
begin
  SetPropertyValue('ch', AValue);
end;

procedure TjsHTMLTableCellElement.Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
begin
  SetPropertyValue('chOff', AValue);
end;

procedure TjsHTMLTableCellElement.Set_colSpan(AValue : LongInt); //property colSpan : LongInt read Get_colSpan write Set_colSpan;
begin
  SetPropertyValue('colSpan', AValue);
end;

procedure TjsHTMLTableCellElement.Set_headers(AValue : DOMString); //property headers : DOMString read Get_headers write Set_headers;
begin
  SetPropertyValue('headers', AValue);
end;

procedure TjsHTMLTableCellElement.Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
begin
  SetPropertyValue('height', AValue);
end;

procedure TjsHTMLTableCellElement.Set_noWrap(AValue : Boolean); //property noWrap : Boolean read Get_noWrap write Set_noWrap;
begin
  SetPropertyValue('noWrap', AValue);
end;

procedure TjsHTMLTableCellElement.Set_rowSpan(AValue : LongInt); //property rowSpan : LongInt read Get_rowSpan write Set_rowSpan;
begin
  SetPropertyValue('rowSpan', AValue);
end;

procedure TjsHTMLTableCellElement.Set_scope(AValue : DOMString); //property scope : DOMString read Get_scope write Set_scope;
begin
  SetPropertyValue('scope', AValue);
end;

procedure TjsHTMLTableCellElement.Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
begin
  SetPropertyValue('vAlign', AValue);
end;

procedure TjsHTMLTableCellElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;
{ TjsHTMLTableRowElement }

procedure TjsHTMLTableRowElement.deleteCell(index: Integer);
begin
  ExecMethod('deleteCell('+ToJSCodeEx(index)+')');
end;

function TjsHTMLTableRowElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLTableRowElement.Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
begin
  GetpropertyValue('bgColor', Result)
end;

function TjsHTMLTableRowElement.Get_cells : IHTMLCollection; //property cells : IHTMLCollection read Get_cells;
begin
  GetPropertyValueIntf('cells', Result, TjsHTMLCollection)
end;

function TjsHTMLTableRowElement.Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
begin
  GetpropertyValue('ch', Result)
end;

function TjsHTMLTableRowElement.Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
begin
  GetpropertyValue('chOff', Result)
end;

function TjsHTMLTableRowElement.Get_rowIndex : LongInt; //property rowIndex : LongInt read Get_rowIndex;
begin
  GetpropertyValue('rowIndex', Result)
end;

function TjsHTMLTableRowElement.Get_sectionRowIndex : LongInt; //property sectionRowIndex : LongInt read Get_sectionRowIndex;
begin
  GetpropertyValue('sectionRowIndex', Result)
end;

function TjsHTMLTableRowElement.Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
begin
  GetpropertyValue('vAlign', Result)
end;

function TjsHTMLTableRowElement.insertCell(index : LongInt) : IHTMLElement;// procedure deleteCell(index : LongInt);
begin
  Result:=ExecMethod('insertCell('+ToJSCodeEx(index)+')', TjsHTMLElement) as IHTMLElement;
end;

procedure TjsHTMLTableRowElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLTableRowElement.Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
begin
  SetPropertyValue('bgColor', AValue);
end;

procedure TjsHTMLTableRowElement.Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
begin
  SetPropertyValue('ch', AValue);
end;

procedure TjsHTMLTableRowElement.Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
begin
  SetPropertyValue('chOff', AValue);
end;

procedure TjsHTMLTableRowElement.Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
begin
  SetPropertyValue('vAlign', AValue);
end;
{ TjsHTMLTableSectionElement }

procedure TjsHTMLTableSectionElement.deleteRow(index: Integer);
begin
  ExecMethod('deleteRow('+ToJSCodeEx(index)+')');
end;

function TjsHTMLTableSectionElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLTableSectionElement.Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
begin
  GetpropertyValue('ch', Result)
end;

function TjsHTMLTableSectionElement.Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
begin
  GetpropertyValue('chOff', Result)
end;

function TjsHTMLTableSectionElement.Get_rows : IHTMLCollection; //property rows : IHTMLCollection read Get_rows;
begin
  GetPropertyValueIntf('rows', Result, TjsHTMLCollection)
end;

function TjsHTMLTableSectionElement.Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
begin
  GetpropertyValue('vAlign', Result)
end;

function TjsHTMLTableSectionElement.insertRow(index : LongInt) : IHTMLElement;// procedure deleteRow(index : LongInt);
begin
  Result:=ExecMethod('insertRow('+ToJSCodeEx(index)+')', TjsHTMLElement) as IHTMLElement;
end;

procedure TjsHTMLTableSectionElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLTableSectionElement.Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
begin
  SetPropertyValue('ch', AValue);
end;

procedure TjsHTMLTableSectionElement.Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
begin
  SetPropertyValue('chOff', AValue);
end;

procedure TjsHTMLTableSectionElement.Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
begin
  SetPropertyValue('vAlign', AValue);
end;
{ TjsHTMLTableColElement }

function TjsHTMLTableColElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLTableColElement.Get_ch : DOMString;// procedure Set_ch(AValue : DOMString);
begin
  GetpropertyValue('ch', Result)
end;

function TjsHTMLTableColElement.Get_chOff : DOMString;// procedure Set_chOff(AValue : DOMString);
begin
  GetpropertyValue('chOff', Result)
end;

function TjsHTMLTableColElement.Get_span : LongInt;// procedure Set_span(AValue : LongInt);
begin
  GetpropertyValue('span', Result)
end;

function TjsHTMLTableColElement.Get_vAlign : DOMString;// procedure Set_vAlign(AValue : DOMString);
begin
  GetpropertyValue('vAlign', Result)
end;

function TjsHTMLTableColElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLTableColElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLTableColElement.Set_ch(AValue : DOMString); //property ch : DOMString read Get_ch write Set_ch;
begin
  SetPropertyValue('ch', AValue);
end;

procedure TjsHTMLTableColElement.Set_chOff(AValue : DOMString); //property chOff : DOMString read Get_chOff write Set_chOff;
begin
  SetPropertyValue('chOff', AValue);
end;

procedure TjsHTMLTableColElement.Set_span(AValue : LongInt); //property span : LongInt read Get_span write Set_span;
begin
  SetPropertyValue('span', AValue);
end;

procedure TjsHTMLTableColElement.Set_vAlign(AValue : DOMString); //property vAlign : DOMString read Get_vAlign write Set_vAlign;
begin
  SetPropertyValue('vAlign', AValue);
end;

procedure TjsHTMLTableColElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLTableCaptionElement }

function TjsHTMLTableCaptionElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLTableCaptionElement.Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
begin
  GetpropertyValue('bgColor', Result)
end;

function TjsHTMLTableCaptionElement.Get_border : DOMString;// procedure Set_border(AValue : DOMString);
begin
  GetpropertyValue('border', Result)
end;

function TjsHTMLTableCaptionElement.Get_caption : IHTMLTableCaptionElement;// procedure Set_caption(AValue : IHTMLTableCaptionElement);
begin
  GetPropertyValueIntf('caption', Result, TjsHTMLTableCaptionElement)
end;

function TjsHTMLTableCaptionElement.Get_cellPadding : DOMString;// procedure Set_cellPadding(AValue : DOMString);
begin
  GetpropertyValue('cellPadding', Result)
end;

function TjsHTMLTableCaptionElement.Get_cellSpacing : DOMString;// procedure Set_cellSpacing(AValue : DOMString);
begin
  GetpropertyValue('cellSpacing', Result)
end;

function TjsHTMLTableCaptionElement.Get_frame : DOMString;// procedure Set_frame(AValue : DOMString);
begin
  GetpropertyValue('frame', Result)
end;

function TjsHTMLTableCaptionElement.Get_rows : IHTMLCollection; //property rows : IHTMLCollection read Get_rows;
begin
  GetPropertyValueIntf('rows', Result, TjsHTMLCollection)
end;

function TjsHTMLTableCaptionElement.Get_rules : DOMString;// procedure Set_rules(AValue : DOMString);
begin
  GetpropertyValue('rules', Result)
end;

function TjsHTMLTableCaptionElement.Get_summary : DOMString;// procedure Set_summary(AValue : DOMString);
begin
  GetpropertyValue('summary', Result)
end;

function TjsHTMLTableCaptionElement.Get_tBodies : IHTMLCollection; //property tBodies : IHTMLCollection read Get_tBodies;
begin
  GetPropertyValueIntf('tBodies', Result, TjsHTMLCollection)
end;

function TjsHTMLTableCaptionElement.Get_tFoot : IHTMLTableSectionElement;// procedure Set_tFoot(AValue : IHTMLTableSectionElement);
begin
  GetPropertyValueIntf('tFoot', Result, TjsHTMLTableSectionElement)
end;

function TjsHTMLTableCaptionElement.Get_tHead : IHTMLTableSectionElement;// procedure Set_tHead(AValue : IHTMLTableSectionElement);
begin
  GetpropertyValueIntf('tHead', Result, TjsHTMLTableSectionElement)
end;

function TjsHTMLTableCaptionElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

function TjsHTMLTableCaptionElement.insertRow(index : LongInt) : IHTMLElement;// procedure deleteRow(index : LongInt);
begin
  Result:=ExecMethod('insertRow('+ToJSCodeEx(index)+')', TjsHTMLElement) as IHTMLElement;
end;

procedure TjsHTMLTableCaptionElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
begin
  SetPropertyValue('bgColor', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_border(AValue : DOMString); //property border : DOMString read Get_border write Set_border;
begin
  SetPropertyValue('border', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_caption(AValue : IHTMLTableCaptionElement); //property caption : IHTMLTableCaptionElement read Get_caption write Set_caption;
begin
  SetPropertyValue('caption', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_cellPadding(AValue : DOMString); //property cellPadding : DOMString read Get_cellPadding write Set_cellPadding;
begin
  SetPropertyValue('cellPadding', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_cellSpacing(AValue : DOMString); //property cellSpacing : DOMString read Get_cellSpacing write Set_cellSpacing;
begin
  SetPropertyValue('cellSpacing', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_frame(AValue : DOMString); //property frame : DOMString read Get_frame write Set_frame;
begin
  SetPropertyValue('frame', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_rules(AValue : DOMString); //property rules : DOMString read Get_rules write Set_rules;
begin
  SetPropertyValue('rules', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_summary(AValue : DOMString); //property summary : DOMString read Get_summary write Set_summary;
begin
  SetPropertyValue('summary', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_tFoot(AValue : IHTMLTableSectionElement); //property tFoot : IHTMLTableSectionElement read Get_tFoot write Set_tFoot;
begin
  SetPropertyValue('tFoot', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_tHead(AValue : IHTMLTableSectionElement); //property tHead : IHTMLTableSectionElement read Get_tHead write Set_tHead;
begin
  SetPropertyValue('tHead', AValue);
end;

procedure TjsHTMLTableCaptionElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLScriptElement }

function TjsHTMLScriptElement.Get_charset : DOMString;// procedure Set_charset(AValue : DOMString);
begin
  GetpropertyValue('charset', Result)
end;

function TjsHTMLScriptElement.Get_defer : Boolean;// procedure Set_defer(AValue : Boolean);
begin
  GetpropertyValue('defer', Result)
end;

function TjsHTMLScriptElement.Get_event : DOMString;// procedure Set_event(AValue : DOMString);
begin
  GetpropertyValue('event', Result)
end;

function TjsHTMLScriptElement.Get_htmlFor : DOMString;// procedure Set_htmlFor(AValue : DOMString);
begin
  GetpropertyValue('htmlFor', Result)
end;

function TjsHTMLScriptElement.Get_src : DOMString;// procedure Set_src(AValue : DOMString);
begin
  GetpropertyValue('src', Result)
end;

function TjsHTMLScriptElement.Get_text : DOMString;// procedure Set_text(AValue : DOMString);
begin
  GetpropertyValue('text', Result)
end;

function TjsHTMLScriptElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

procedure TjsHTMLScriptElement.Set_charset(AValue : DOMString); //property charset : DOMString read Get_charset write Set_charset;
begin
  SetPropertyValue('charset', AValue);
end;

procedure TjsHTMLScriptElement.Set_defer(AValue : Boolean); //property defer : Boolean read Get_defer write Set_defer;
begin
  SetPropertyValue('defer', AValue);
end;

procedure TjsHTMLScriptElement.Set_event(AValue : DOMString); //property event : DOMString read Get_event write Set_event;
begin
  SetPropertyValue('event', AValue);
end;

procedure TjsHTMLScriptElement.Set_htmlFor(AValue : DOMString); //property htmlFor : DOMString read Get_htmlFor write Set_htmlFor;
begin
  SetPropertyValue('htmlFor', AValue);
end;

procedure TjsHTMLScriptElement.Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
begin
  SetPropertyValue('src', AValue);
end;

procedure TjsHTMLScriptElement.Set_text(AValue : DOMString); //property text : DOMString read Get_text write Set_text;
begin
  SetPropertyValue('text', AValue);
end;

procedure TjsHTMLScriptElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;
{ TjsHTMLAreaElement }

function TjsHTMLAreaElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLAreaElement.Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
begin
  GetpropertyValue('alt', Result)
end;

function TjsHTMLAreaElement.Get_coords : DOMString;// procedure Set_coords(AValue : DOMString);
begin
  GetpropertyValue('coords', Result)
end;

function TjsHTMLAreaElement.Get_href : DOMString;// procedure Set_href(AValue : DOMString);
begin
  GetpropertyValue('href', Result)
end;

function TjsHTMLAreaElement.Get_noHref : Boolean;// procedure Set_noHref(AValue : Boolean);
begin
  GetpropertyValue('noHref', Result)
end;

function TjsHTMLAreaElement.Get_shape : DOMString;// procedure Set_shape(AValue : DOMString);
begin
  GetpropertyValue('shape', Result)
end;

function TjsHTMLAreaElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLAreaElement.Get_target : DOMString;// procedure Set_target(AValue : DOMString);
begin
  GetpropertyValue('target', Result)
end;

procedure TjsHTMLAreaElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLAreaElement.Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
begin
  SetPropertyValue('alt', AValue);
end;

procedure TjsHTMLAreaElement.Set_coords(AValue : DOMString); //property coords : DOMString read Get_coords write Set_coords;
begin
  SetPropertyValue('coords', AValue);
end;

procedure TjsHTMLAreaElement.Set_href(AValue : DOMString); //property href : DOMString read Get_href write Set_href;
begin
  SetPropertyValue('href', AValue);
end;

procedure TjsHTMLAreaElement.Set_noHref(AValue : Boolean); //property noHref : Boolean read Get_noHref write Set_noHref;
begin
  SetPropertyValue('noHref', AValue);
end;

procedure TjsHTMLAreaElement.Set_shape(AValue : DOMString); //property shape : DOMString read Get_shape write Set_shape;
begin
  SetPropertyValue('shape', AValue);
end;

procedure TjsHTMLAreaElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLAreaElement.Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
begin
  SetPropertyValue('target', AValue);
end;

{ TjsHTMLMapElement }

function TjsHTMLMapElement.Get_areas : IHTMLCollection; //property areas : IHTMLCollection read Get_areas;
begin
  GetPropertyValueIntf('areas', Result, TjsHTMLCollection);
end;

function TjsHTMLMapElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

procedure TjsHTMLMapElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

{ TjsHTMLAppletElement }

function TjsHTMLAppletElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLAppletElement.Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
begin
  GetpropertyValue('alt', Result)
end;

function TjsHTMLAppletElement.Get_archive : DOMString;// procedure Set_archive(AValue : DOMString);
begin
  GetpropertyValue('archive', Result)
end;

function TjsHTMLAppletElement.Get_code : DOMString;// procedure Set_code(AValue : DOMString);
begin
  GetpropertyValue('code', Result)
end;

function TjsHTMLAppletElement.Get_codeBase : DOMString;// procedure Set_codeBase(AValue : DOMString);
begin
  GetpropertyValue('codeBase', Result)
end;

function TjsHTMLAppletElement.Get_height : DOMString;// procedure Set_height(AValue : DOMString);
begin
  GetpropertyValue('height', Result)
end;

function TjsHTMLAppletElement.Get_hspace : LongInt;// procedure Set_hspace(AValue : LongInt);
begin
  GetpropertyValue('hspace', Result)
end;

function TjsHTMLAppletElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLAppletElement.Get_object : DOMString;// procedure Set_object(AValue : DOMString);
begin
  GetpropertyValue('object', Result)
end;

function TjsHTMLAppletElement.Get_vspace : LongInt;// procedure Set_vspace(AValue : LongInt);
begin
  GetpropertyValue('vspace', Result)
end;

function TjsHTMLAppletElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLAppletElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLAppletElement.Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
begin
  SetPropertyValue('alt', AValue);
end;

procedure TjsHTMLAppletElement.Set_archive(AValue : DOMString); //property archive : DOMString read Get_archive write Set_archive;
begin
  SetPropertyValue('archive', AValue);
end;

procedure TjsHTMLAppletElement.Set_code(AValue : DOMString); //property code : DOMString read Get_code write Set_code;
begin
  SetPropertyValue('code', AValue);
end;

procedure TjsHTMLAppletElement.Set_codeBase(AValue : DOMString); //property codeBase : DOMString read Get_codeBase write Set_codeBase;
begin
  SetPropertyValue('codeBase', AValue);
end;

procedure TjsHTMLAppletElement.Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
begin
  SetPropertyValue('height', AValue);
end;

procedure TjsHTMLAppletElement.Set_hspace(AValue : LongInt); //property hspace : LongInt read Get_hspace write Set_hspace;
begin
  SetPropertyValue('hspace', AValue);
end;

procedure TjsHTMLAppletElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLAppletElement.Set_object(AValue : DOMString); //property _object : DOMString read Get_object write Set_object;
begin
  SetPropertyValue('object', AValue);
end;

procedure TjsHTMLAppletElement.Set_vspace(AValue : LongInt); //property vspace : LongInt read Get_vspace write Set_vspace;
begin
  SetPropertyValue('vspace', AValue);
end;

procedure TjsHTMLAppletElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLParamElement }

function TjsHTMLParamElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLParamElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLParamElement.Get_value : DOMString;// procedure Set_value(AValue : DOMString);
begin
  GetpropertyValue('value', Result)
end;

function TjsHTMLParamElement.Get_valueType : DOMString;// procedure Set_valueType(AValue : DOMString);
begin
  GetpropertyValue('valueType', Result)
end;

procedure TjsHTMLParamElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLParamElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

procedure TjsHTMLParamElement.Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

procedure TjsHTMLParamElement.Set_valueType(AValue : DOMString); //property valueType : DOMString read Get_valueType write Set_valueType;
begin
  SetPropertyValue('valueType', AValue);
end;

{ TjsHTMLObjectElement }

function TjsHTMLObjectElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLObjectElement.Get_archive : DOMString;// procedure Set_archive(AValue : DOMString);
begin
  GetpropertyValue('archive', Result)
end;

function TjsHTMLObjectElement.Get_border : DOMString;// procedure Set_border(AValue : DOMString);
begin
  GetpropertyValue('border', Result)
end;

function TjsHTMLObjectElement.Get_code : DOMString;// procedure Set_code(AValue : DOMString);
begin
  GetpropertyValue('code', Result)
end;

function TjsHTMLObjectElement.Get_codeBase : DOMString;// procedure Set_codeBase(AValue : DOMString);
begin
  GetpropertyValue('codeBase', Result)
end;

function TjsHTMLObjectElement.Get_codeType : DOMString;// procedure Set_codeType(AValue : DOMString);
begin
  GetpropertyValue('codeType', Result)
end;

function TjsHTMLObjectElement.Get_contentDocument : IHTMLDocument; //property contentDocument : IHTMLDocument read Get_contentDocument;
begin
  GetPropertyValueIntf('contentDocument', Result, TjsHTMLDocument)
end;

function TjsHTMLObjectElement.Get_data : DOMString;// procedure Set_data(AValue : DOMString);
begin
  GetpropertyValue('data', Result)
end;

function TjsHTMLObjectElement.Get_declare : Boolean;// procedure Set_declare(AValue : Boolean);
begin
  GetpropertyValue('declare', Result)
end;

function TjsHTMLObjectElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result,TjsHTMLFormElement)
end;

function TjsHTMLObjectElement.Get_height : DOMString;// procedure Set_height(AValue : DOMString);
begin
  GetpropertyValue('height', Result)
end;

function TjsHTMLObjectElement.Get_hspace : LongInt;// procedure Set_hspace(AValue : LongInt);
begin
  GetpropertyValue('hspace', Result)
end;

function TjsHTMLObjectElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLObjectElement.Get_standby : DOMString;// procedure Set_standby(AValue : DOMString);
begin
  GetpropertyValue('standby', Result)
end;

function TjsHTMLObjectElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLObjectElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLObjectElement.Get_useMap : DOMString;// procedure Set_useMap(AValue : DOMString);
begin
  GetpropertyValue('useMap', Result)
end;

function TjsHTMLObjectElement.Get_vspace : LongInt;// procedure Set_vspace(AValue : LongInt);
begin
  GetpropertyValue('vspace', Result)
end;

function TjsHTMLObjectElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLObjectElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLObjectElement.Set_archive(AValue : DOMString); //property archive : DOMString read Get_archive write Set_archive;
begin
  SetPropertyValue('archive', AValue);
end;

procedure TjsHTMLObjectElement.Set_border(AValue : DOMString); //property border : DOMString read Get_border write Set_border;
begin
  SetPropertyValue('border', AValue);
end;

procedure TjsHTMLObjectElement.Set_code(AValue : DOMString); //property code : DOMString read Get_code write Set_code;
begin
  SetPropertyValue('code', AValue);
end;

procedure TjsHTMLObjectElement.Set_codeBase(AValue : DOMString); //property codeBase : DOMString read Get_codeBase write Set_codeBase;
begin
  SetPropertyValue('codeBase', AValue);
end;

procedure TjsHTMLObjectElement.Set_codeType(AValue : DOMString); //property codeType : DOMString read Get_codeType write Set_codeType;
begin
  SetPropertyValue('codeType', AValue);
end;

procedure TjsHTMLObjectElement.Set_data(AValue : DOMString); //property data : DOMString read Get_data write Set_data;
begin
  SetPropertyValue('data', AValue);
end;

procedure TjsHTMLObjectElement.Set_declare(AValue : Boolean); //property declare : Boolean read Get_declare write Set_declare;
begin
  SetPropertyValue('declare', AValue);
end;

procedure TjsHTMLObjectElement.Set_height(AValue : DOMString); //property height : DOMString read Get_height write Set_height;
begin
  SetPropertyValue('height', AValue);
end;

procedure TjsHTMLObjectElement.Set_hspace(AValue : LongInt); //property hspace : LongInt read Get_hspace write Set_hspace;
begin
  SetPropertyValue('hspace', AValue);
end;

procedure TjsHTMLObjectElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLObjectElement.Set_standby(AValue : DOMString); //property standby : DOMString read Get_standby write Set_standby;
begin
  SetPropertyValue('standby', AValue);
end;

procedure TjsHTMLObjectElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLObjectElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

procedure TjsHTMLObjectElement.Set_useMap(AValue : DOMString); //property useMap : DOMString read Get_useMap write Set_useMap;
begin
  SetPropertyValue('useMap', AValue);
end;

procedure TjsHTMLObjectElement.Set_vspace(AValue : LongInt); //property vspace : LongInt read Get_vspace write Set_vspace;
begin
  SetPropertyValue('vspace', AValue);
end;

procedure TjsHTMLObjectElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLImageElement }

function TjsHTMLImageElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLImageElement.Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
begin
  GetpropertyValue('alt', Result)
end;

function TjsHTMLImageElement.Get_border : DOMString;// procedure Set_border(AValue : DOMString);
begin
  GetpropertyValue('border', Result)
end;

function TjsHTMLImageElement.Get_height : LongInt;// procedure Set_height(AValue : LongInt);
begin
  GetpropertyValue('height', Result)
end;

function TjsHTMLImageElement.Get_hspace : LongInt;// procedure Set_hspace(AValue : LongInt);
begin
  GetpropertyValue('hspace', Result)
end;

function TjsHTMLImageElement.Get_isMap : Boolean;// procedure Set_isMap(AValue : Boolean);
begin
  GetpropertyValue('isMap', Result)
end;

function TjsHTMLImageElement.Get_longDesc : DOMString;// procedure Set_longDesc(AValue : DOMString);
begin
  GetpropertyValue('longDesc', Result)
end;

function TjsHTMLImageElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLImageElement.Get_src : DOMString;// procedure Set_src(AValue : DOMString);
begin
  GetpropertyValue('src', Result)
end;

function TjsHTMLImageElement.Get_useMap : DOMString;// procedure Set_useMap(AValue : DOMString);
begin
  GetpropertyValue('useMap', Result)
end;

function TjsHTMLImageElement.Get_vspace : LongInt;// procedure Set_vspace(AValue : LongInt);
begin
  GetpropertyValue('vspace', Result)
end;

function TjsHTMLImageElement.Get_width : LongInt;// procedure Set_width(AValue : LongInt);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLImageElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLImageElement.Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
begin
  SetPropertyValue('alt', AValue);
end;

procedure TjsHTMLImageElement.Set_border(AValue : DOMString); //property border : DOMString read Get_border write Set_border;
begin
  SetPropertyValue('border', AValue);
end;

procedure TjsHTMLImageElement.Set_height(AValue : LongInt); //property height : LongInt read Get_height write Set_height;
begin
  SetPropertyValue('height', AValue);
end;

procedure TjsHTMLImageElement.Set_hspace(AValue : LongInt); //property hspace : LongInt read Get_hspace write Set_hspace;
begin
  SetPropertyValue('hspace', AValue);
end;

procedure TjsHTMLImageElement.Set_isMap(AValue : Boolean); //property isMap : Boolean read Get_isMap write Set_isMap;
begin
  SetPropertyValue('isMap', AValue);
end;

procedure TjsHTMLImageElement.Set_longDesc(AValue : DOMString); //property longDesc : DOMString read Get_longDesc write Set_longDesc;
begin
  SetPropertyValue('longDesc', AValue);
end;

procedure TjsHTMLImageElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLImageElement.Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
begin
  SetPropertyValue('src', AValue);
end;

procedure TjsHTMLImageElement.Set_useMap(AValue : DOMString); //property useMap : DOMString read Get_useMap write Set_useMap;
begin
  SetPropertyValue('useMap', AValue);
end;

procedure TjsHTMLImageElement.Set_vspace(AValue : LongInt); //property vspace : LongInt read Get_vspace write Set_vspace;
begin
  SetPropertyValue('vspace', AValue);
end;

procedure TjsHTMLImageElement.Set_width(AValue : LongInt); //property width : LongInt read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLAnchorElement }

procedure TjsHTMLAnchorElement.blur;
begin
  ExecMethod('blur');
end;

procedure TjsHTMLAnchorElement.focus;
begin
  ExecMethod('focus')
end;

function TjsHTMLAnchorElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLAnchorElement.Get_charset : DOMString;// procedure Set_charset(AValue : DOMString);
begin
  GetpropertyValue('charset', Result)
end;

function TjsHTMLAnchorElement.Get_coords : DOMString;// procedure Set_coords(AValue : DOMString);
begin
  GetpropertyValue('coords', Result)
end;

function TjsHTMLAnchorElement.Get_href : DOMString;// procedure Set_href(AValue : DOMString);
begin
  GetpropertyValue('href', Result)
end;

function TjsHTMLAnchorElement.Get_hreflang : DOMString;// procedure Set_hreflang(AValue : DOMString);
begin
  GetpropertyValue('hreflang', Result)
end;

function TjsHTMLAnchorElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLAnchorElement.Get_rel : DOMString;// procedure Set_rel(AValue : DOMString);
begin
  GetpropertyValue('rel', Result)
end;

function TjsHTMLAnchorElement.Get_rev : DOMString;// procedure Set_rev(AValue : DOMString);
begin
  GetpropertyValue('rev', Result)
end;

function TjsHTMLAnchorElement.Get_shape : DOMString;// procedure Set_shape(AValue : DOMString);
begin
  GetpropertyValue('shape', Result)
end;

function TjsHTMLAnchorElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLAnchorElement.Get_target : DOMString;// procedure Set_target(AValue : DOMString);
begin
  GetpropertyValue('target', Result)
end;

function TjsHTMLAnchorElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

procedure TjsHTMLAnchorElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLAnchorElement.Set_charset(AValue : DOMString); //property charset : DOMString read Get_charset write Set_charset;
begin
  SetPropertyValue('charset', AValue);
end;

procedure TjsHTMLAnchorElement.Set_coords(AValue : DOMString); //property coords : DOMString read Get_coords write Set_coords;
begin
  SetPropertyValue('coords', AValue);
end;

procedure TjsHTMLAnchorElement.Set_href(AValue : DOMString); //property href : DOMString read Get_href write Set_href;
begin
  SetPropertyValue('href', AValue);
end;

procedure TjsHTMLAnchorElement.Set_hreflang(AValue : DOMString); //property hreflang : DOMString read Get_hreflang write Set_hreflang;
begin
  SetPropertyValue('hreflang', AValue);
end;

procedure TjsHTMLAnchorElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLAnchorElement.Set_rel(AValue : DOMString); //property rel : DOMString read Get_rel write Set_rel;
begin
  SetPropertyValue('rel', AValue);
end;

procedure TjsHTMLAnchorElement.Set_rev(AValue : DOMString); //property rev : DOMString read Get_rev write Set_rev;
begin
  SetPropertyValue('rev', AValue);
end;

procedure TjsHTMLAnchorElement.Set_shape(AValue : DOMString); //property shape : DOMString read Get_shape write Set_shape;
begin
  SetPropertyValue('shape', AValue);
end;

procedure TjsHTMLAnchorElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLAnchorElement.Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
begin
  SetPropertyValue('target', AValue);
end;

procedure TjsHTMLAnchorElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

{ TjsHTMLModElement }

function TjsHTMLModElement.Get_cite : DOMString;// procedure Set_cite(AValue : DOMString);
begin
  GetpropertyValue('cite', Result)
end;

function TjsHTMLModElement.Get_dateTime : DOMString;// procedure Set_dateTime(AValue : DOMString);
begin
  GetpropertyValue('dateTime', Result)
end;

procedure TjsHTMLModElement.Set_cite(AValue : DOMString); //property cite : DOMString read Get_cite write Set_cite;
begin
  SetPropertyValue('cite', AValue);
end;

procedure TjsHTMLModElement.Set_dateTime(AValue : DOMString); //property dateTime : DOMString read Get_dateTime write Set_dateTime;
begin
  SetPropertyValue('dateTime', AValue);
end;

{ TjsHTMLHRElement }

function TjsHTMLHRElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLHRElement.Get_noShade : Boolean;// procedure Set_noShade(AValue : Boolean);
begin
  GetpropertyValue('noShade', Result)
end;

function TjsHTMLHRElement.Get_size : DOMString;// procedure Set_size(AValue : DOMString);
begin
  GetpropertyValue('size', Result)
end;

function TjsHTMLHRElement.Get_width : DOMString;// procedure Set_width(AValue : DOMString);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLHRElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLHRElement.Set_noShade(AValue : Boolean); //property noShade : Boolean read Get_noShade write Set_noShade;
begin
  SetPropertyValue('noShade', AValue);
end;

procedure TjsHTMLHRElement.Set_size(AValue : DOMString); //property size : DOMString read Get_size write Set_size;
begin
  SetPropertyValue('size', AValue);
end;

procedure TjsHTMLHRElement.Set_width(AValue : DOMString); //property width : DOMString read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLFontElement }

function TjsHTMLFontElement.Get_color : DOMString;// procedure Set_color(AValue : DOMString);
begin
  GetpropertyValue('color', Result)
end;

function TjsHTMLFontElement.Get_face : DOMString;// procedure Set_face(AValue : DOMString);
begin
  GetpropertyValue('face', Result)
end;

function TjsHTMLFontElement.Get_size : DOMString;// procedure Set_size(AValue : DOMString);
begin
  GetpropertyValue('size', Result)
end;

procedure TjsHTMLFontElement.Set_color(AValue : DOMString); //property color : DOMString read Get_color write Set_color;
begin
  SetPropertyValue('color', AValue);
end;

procedure TjsHTMLFontElement.Set_face(AValue : DOMString); //property face : DOMString read Get_face write Set_face;
begin
  SetPropertyValue('face', AValue);
end;

procedure TjsHTMLFontElement.Set_size(AValue : DOMString); //property size : DOMString read Get_size write Set_size;
begin
  SetPropertyValue('size', AValue);
end;

{ TjsHTMLBaseFontElement }

function TjsHTMLBaseFontElement.Get_color : DOMString;// procedure Set_color(AValue : DOMString);
begin
  GetpropertyValue('color', Result)
end;

function TjsHTMLBaseFontElement.Get_face : DOMString;// procedure Set_face(AValue : DOMString);
begin
  GetpropertyValue('face', Result)
end;

function TjsHTMLBaseFontElement.Get_size : LongInt;// procedure Set_size(AValue : LongInt);
begin
  GetpropertyValue('size', Result)
end;

procedure TjsHTMLBaseFontElement.Set_color(AValue : DOMString); //property color : DOMString read Get_color write Set_color;
begin
  SetPropertyValue('color', AValue);
end;

procedure TjsHTMLBaseFontElement.Set_face(AValue : DOMString); //property face : DOMString read Get_face write Set_face;
begin
  SetPropertyValue('face', AValue);
end;

procedure TjsHTMLBaseFontElement.Set_size(AValue : LongInt); //property size : LongInt read Get_size write Set_size;
begin
  SetPropertyValue('size', AValue);
end;

 { TjsHTMLBRElement }

function TjsHTMLBRElement.Get_clear : DOMString;// procedure Set_clear(AValue : DOMString);
begin
  GetpropertyValue('clear', Result)
end;

procedure TjsHTMLBRElement.Set_clear(AValue : DOMString); //property clear : DOMString read Get_clear write Set_clear;
begin
  SetPropertyValue('clear', AValue);
end;

{ TjsHTMLPreElement }

function TjsHTMLPreElement.Get_width : LongInt;// procedure Set_width(AValue : LongInt);
begin
  GetpropertyValue('width', Result)
end;

procedure TjsHTMLPreElement.Set_width(AValue : LongInt); //property width : LongInt read Get_width write Set_width;
begin
  SetPropertyValue('width', AValue);
end;

{ TjsHTMLQuoteElement }

function TjsHTMLQuoteElement.Get_cite : DOMString;// procedure Set_cite(AValue : DOMString);
begin
  GetpropertyValue('cite', Result)
end;

procedure TjsHTMLQuoteElement.Set_cite(AValue : DOMString); //property cite : DOMString read Get_cite write Set_cite;
begin
  SetPropertyValue('cite', AValue);
end;

{ TjsHTMLHeadingElement }

function TjsHTMLHeadingElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

procedure TjsHTMLHeadingElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

{ TjsHTMLParagraphElement }

function TjsHTMLParagraphElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

procedure TjsHTMLParagraphElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

{ TjsHTMLDivElement }

function TjsHTMLDivElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

procedure TjsHTMLDivElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

{ TjsHTMLLIElement }

function TjsHTMLLIElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLLIElement.Get_value : LongInt;// procedure Set_value(AValue : LongInt);
begin
  GetpropertyValue('value', Result)
end;

procedure TjsHTMLLIElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

procedure TjsHTMLLIElement.Set_value(AValue : LongInt); //property value : LongInt read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

{ TjsHTMLMenuElement }

function TjsHTMLMenuElement.Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
begin
  GetpropertyValue('compact', Result)
end;

procedure TjsHTMLMenuElement.Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
begin
  SetPropertyValue('compact', AValue);
end;

{ TjsHTMLDirectoryElement }

function TjsHTMLDirectoryElement.Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
begin
  GetpropertyValue('compact', Result)
end;

procedure TjsHTMLDirectoryElement.Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
begin
  SetPropertyValue('compact', AValue);
end;

{ TjsHTMLDListElement }

function TjsHTMLDListElement.Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
begin
  GetpropertyValue('compact', Result)
end;

procedure TjsHTMLDListElement.Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
begin
  SetPropertyValue('compact', AValue);
end;

{ TjsHTMLOListElement }

function TjsHTMLOListElement.Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
begin
  GetpropertyValue('compact', Result)
end;

function TjsHTMLOListElement.Get_start : LongInt;// procedure Set_start(AValue : LongInt);
begin
  GetpropertyValue('start', Result)
end;

function TjsHTMLOListElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

procedure TjsHTMLOListElement.Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
begin
  SetPropertyValue('compact', AValue);
end;

procedure TjsHTMLOListElement.Set_start(AValue : LongInt); //property start : LongInt read Get_start write Set_start;
begin
  SetPropertyValue('start', AValue);
end;

procedure TjsHTMLOListElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

{ TjsHTMLUListElement }

function TjsHTMLUListElement.Get_compact : Boolean;// procedure Set_compact(AValue : Boolean);
begin
  GetpropertyValue('compact', Result)
end;

function TjsHTMLUListElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

procedure TjsHTMLUListElement.Set_compact(AValue : Boolean); //property compact : Boolean read Get_compact write Set_compact;
begin
  SetPropertyValue('compact', AValue);
end;

procedure TjsHTMLUListElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

{ TjsHTMLLegendElement }

function TjsHTMLLegendElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLLegendElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLLegendElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

procedure TjsHTMLLegendElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLLegendElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

{ TjsHTMLFieldSetElement }

function TjsHTMLFieldSetElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

{ TjsHTMLLabelElement }

function TjsHTMLLabelElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLLabelElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLLabelElement.Get_htmlFor : DOMString;// procedure Set_htmlFor(AValue : DOMString);
begin
  GetpropertyValue('htmlFor', Result)
end;

procedure TjsHTMLLabelElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLLabelElement.Set_htmlFor(AValue : DOMString); //property htmlFor : DOMString read Get_htmlFor write Set_htmlFor;
begin
  SetPropertyValue('htmlFor', AValue);
end;

{ TjsHTMLButtonElement }

function TjsHTMLButtonElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLButtonElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLButtonElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLButtonElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLButtonElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLButtonElement.Get_type : DOMString; //property _type : DOMString read Get_type;
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLButtonElement.Get_value : DOMString;// procedure Set_value(AValue : DOMString);
begin
  GetpropertyValue('value', Result)
end;

procedure TjsHTMLButtonElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLButtonElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLButtonElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLButtonElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLButtonElement.Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

{ TjsHTMLTextAreaElement }

procedure TjsHTMLTextAreaElement.blur;
begin
  ExecMethod('blur');
end;

procedure TjsHTMLTextAreaElement.focus;
begin
  ExecMethod('focus');
end;

function TjsHTMLTextAreaElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLTextAreaElement.Get_cols : LongInt;// procedure Set_cols(AValue : LongInt);
begin
  GetpropertyValue('cols', Result)
end;

function TjsHTMLTextAreaElement.Get_defaultValue : DOMString;// procedure Set_defaultValue(AValue : DOMString);
begin
  GetpropertyValue('defaultValue', Result)
end;

function TjsHTMLTextAreaElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLTextAreaElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLTextAreaElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLTextAreaElement.Get_readOnly : Boolean;// procedure Set_readOnly(AValue : Boolean);
begin
  GetpropertyValue('readOnly', Result)
end;

function TjsHTMLTextAreaElement.Get_rows : LongInt;// procedure Set_rows(AValue : LongInt);
begin
  GetpropertyValue('rows', Result)
end;

function TjsHTMLTextAreaElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLTextAreaElement.Get_type : DOMString; //property _type : DOMString read Get_type;
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLTextAreaElement.Get_value : DOMString;// procedure Set_value(AValue : DOMString);
begin
  GetpropertyValue('value', Result)
end;

procedure TjsHTMLTextAreaElement.select;
begin
  ExecMethod('select');
end;

procedure TjsHTMLTextAreaElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_cols(AValue : LongInt); //property cols : LongInt read Get_cols write Set_cols;
begin
  SetPropertyValue('cols', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_defaultValue(AValue : DOMString); //property defaultValue : DOMString read Get_defaultValue write Set_defaultValue;
begin
  SetPropertyValue('defaultValue', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_readOnly(AValue : Boolean); //property readOnly : Boolean read Get_readOnly write Set_readOnly;
begin
  SetPropertyValue('readOnly', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_rows(AValue : LongInt); //property rows : LongInt read Get_rows write Set_rows;
begin
  SetPropertyValue('rows', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLTextAreaElement.Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

{ TjsHTMLInputElement }

procedure TjsHTMLInputElement.blur;
begin
  ExecMethod('blur');
end;

procedure TjsHTMLInputElement.click;
begin
  ExecMethod('click');
end;

procedure TjsHTMLInputElement.focus;
begin
  ExecMethod('focus');
end;

function TjsHTMLInputElement.Get_accept : DOMString;// procedure Set_accept(AValue : DOMString);
begin
  GetpropertyValue('accept', Result)
end;

function TjsHTMLInputElement.Get_accessKey : DOMString;// procedure Set_accessKey(AValue : DOMString);
begin
  GetpropertyValue('accessKey', Result)
end;

function TjsHTMLInputElement.Get_align : DOMString;// procedure Set_align(AValue : DOMString);
begin
  GetpropertyValue('align', Result)
end;

function TjsHTMLInputElement.Get_alt : DOMString;// procedure Set_alt(AValue : DOMString);
begin
  GetpropertyValue('alt', Result)
end;

function TjsHTMLInputElement.Get_checked : Boolean;// procedure Set_checked(AValue : Boolean);
begin
  GetpropertyValue('checked', Result)
end;

function TjsHTMLInputElement.Get_defaultChecked : Boolean;// procedure Set_defaultChecked(AValue : Boolean);
begin
  GetpropertyValue('defaultChecked', Result)
end;

function TjsHTMLInputElement.Get_defaultValue : DOMString;// procedure Set_defaultValue(AValue : DOMString);
begin
  GetpropertyValue('defaultValue', Result)
end;

function TjsHTMLInputElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLInputElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLInputElement.Get_maxLength : LongInt;// procedure Set_maxLength(AValue : LongInt);
begin
  GetpropertyValue('maxLength', Result)
end;

function TjsHTMLInputElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLInputElement.Get_readOnly : Boolean;// procedure Set_readOnly(AValue : Boolean);
begin
  GetpropertyValue('readOnly', Result)
end;

function TjsHTMLInputElement.Get_src : DOMString;// procedure Set_src(AValue : DOMString);
begin
  GetpropertyValue('src', Result)
end;

function TjsHTMLInputElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLInputElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLInputElement.Get_useMap : DOMString;// procedure Set_useMap(AValue : DOMString);
begin
  GetpropertyValue('useMap', Result)
end;

function TjsHTMLInputElement.Get_value : DOMString;// procedure Set_value(AValue : DOMString);
begin
  GetpropertyValue('value', Result)
end;

procedure TjsHTMLInputElement.select;
begin
  ExecMethod('select');
end;

procedure TjsHTMLInputElement.Set_accept(AValue : DOMString); //property accept : DOMString read Get_accept write Set_accept;
begin
  SetPropertyValue('accept', AValue);
end;

procedure TjsHTMLInputElement.Set_accessKey(AValue : DOMString); //property accessKey : DOMString read Get_accessKey write Set_accessKey;
begin
  SetPropertyValue('accessKey', AValue);
end;

procedure TjsHTMLInputElement.Set_align(AValue : DOMString); //property align : DOMString read Get_align write Set_align;
begin
  SetPropertyValue('align', AValue);
end;

procedure TjsHTMLInputElement.Set_alt(AValue : DOMString); //property alt : DOMString read Get_alt write Set_alt;
begin
  SetPropertyValue('alt', AValue);
end;

procedure TjsHTMLInputElement.Set_checked(AValue : Boolean); //property checked : Boolean read Get_checked write Set_checked;
begin
  SetPropertyValue('checked', AValue);
end;

procedure TjsHTMLInputElement.Set_defaultChecked(AValue : Boolean); //property defaultChecked : Boolean read Get_defaultChecked write Set_defaultChecked;
begin
  SetPropertyValue('defaultChecked', AValue);
end;

procedure TjsHTMLInputElement.Set_defaultValue(AValue : DOMString); //property defaultValue : DOMString read Get_defaultValue write Set_defaultValue;
begin
  SetPropertyValue('defaultValue', AValue);
end;

procedure TjsHTMLInputElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLInputElement.Set_maxLength(AValue : LongInt); //property maxLength : LongInt read Get_maxLength write Set_maxLength;
begin
  SetPropertyValue('maxLength', AValue);
end;

procedure TjsHTMLInputElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLInputElement.Set_readOnly(AValue : Boolean); //property _readOnly : Boolean read Get_readOnly write Set_readOnly;
begin
  SetPropertyValue('readOnly', AValue);
end;

procedure TjsHTMLInputElement.Set_src(AValue : DOMString); //property src : DOMString read Get_src write Set_src;
begin
  SetPropertyValue('src', AValue);
end;

procedure TjsHTMLInputElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLInputElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

procedure TjsHTMLInputElement.Set_useMap(AValue : DOMString); //property useMap : DOMString read Get_useMap write Set_useMap;
begin
  SetPropertyValue('useMap', AValue);
end;

procedure TjsHTMLInputElement.Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

{ TjsHTMLOptionElement }

function TjsHTMLOptionElement.Get_defaultSelected : Boolean;// procedure Set_defaultSelected(AValue : Boolean);
begin
  GetpropertyValue('defaultSelected', Result)
end;

function TjsHTMLOptionElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLOptionElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLOptionElement.Get_index : LongInt; //property _index : longInt read Get_index;
begin
  GetpropertyValue('index', Result)
end;

function TjsHTMLOptionElement.Get_label : DOMString;// procedure Set_label(AValue : DOMString);
begin
  GetpropertyValue('label', Result)
end;

function TjsHTMLOptionElement.Get_selected : Boolean;// procedure Set_selected(AValue : Boolean);
begin
  GetpropertyValue('selected', Result)
end;

function TjsHTMLOptionElement.Get_text : DOMString; //property text : DOMString read Get_text;
begin
  GetpropertyValue('text', Result)
end;

function TjsHTMLOptionElement.Get_value : DOMString;// procedure Set_value(AValue : DOMString);
begin
  GetpropertyValue('value', Result)
end;

procedure TjsHTMLOptionElement.Set_defaultSelected(AValue : Boolean); //property defaultSelected : Boolean read Get_defaultSelected write Set_defaultSelected;
begin
  SetPropertyValue('defaultSelected', AValue);
end;

procedure TjsHTMLOptionElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLOptionElement.Set_label(AValue : DOMString); //property _label : DOMString read Get_label write Set_label;
begin
  SetPropertyValue('label', AValue);
end;

procedure TjsHTMLOptionElement.Set_selected(AValue : Boolean); //property selected : Boolean read Get_selected write Set_selected;
begin
  SetPropertyValue('selected', AValue);
end;

procedure TjsHTMLOptionElement.Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

{ TjsHTMLOptGroupElement }

function TjsHTMLOptGroupElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLOptGroupElement.Get_label : DOMString;// procedure Set_label(AValue : DOMString);
begin
  GetpropertyValue('label', Result)
end;

procedure TjsHTMLOptGroupElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLOptGroupElement.Set_label(AValue : DOMString); //property _label : DOMString read Get_label write Set_label;
begin
  SetPropertyValue('label', AValue);
end;

{ TjsHTMLSelectElement }

procedure TjsHTMLSelectElement.blur;
begin
  ExecMethod('blur');
end;

procedure TjsHTMLSelectElement.focus;
begin
  ExecMethod('focus');
end;

function TjsHTMLSelectElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLSelectElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLSelectElement.Get_multiple : Boolean;// procedure Set_multiple(AValue : Boolean);
begin
  GetpropertyValue('multiple', Result)
end;

function TjsHTMLSelectElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLSelectElement.Get_options : IHTMLOptionsCollection; //property options : IHTMLOptionsCollection read Get_options;
begin
  GetPropertyValueIntf('options', Result, TjsHTMLOptionsCollection)
end;

function TjsHTMLSelectElement.Get_selectedIndex : LongInt;// procedure Set_selectedIndex(AValue : LongInt);
begin
  GetpropertyValue('selectedIndex', Result)
end;

function TjsHTMLSelectElement.Get_size : LongInt;// procedure Set_size(AValue : LongInt);
begin
  GetpropertyValue('size', Result)
end;

function TjsHTMLSelectElement.Get_tabIndex : LongInt;// procedure Set_tabIndex(AValue : LongInt);
begin
  GetpropertyValue('tabIndex', Result)
end;

function TjsHTMLSelectElement.Get_type : DOMString; //property _type : DOMString read Get_type;
begin
  GetpropertyValue('type', Result)
end;

function TjsHTMLSelectElement.Get_value : DOMString;// procedure Set_value(AValue : DOMString);
begin
  GetpropertyValue('value', Result)
end;

procedure TjsHTMLSelectElement.remove(index: Integer);
begin
  ExecMethod('remove('+ToJSCodeEx(index)+')');
end;

procedure TjsHTMLSelectElement.add(element : IHTMLElement; before : IHTMLElement);// procedure remove(index : LongInt);
begin
  ExecMethod('add('+ToJSCodeEx(element)+','+ToJSCodeEx(before)+')');
end;

procedure TjsHTMLSelectElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLSelectElement.Set_multiple(AValue : Boolean); //property multiple : Boolean read Get_multiple write Set_multiple;
begin
  SetPropertyValue('multiple', AValue);
end;

procedure TjsHTMLSelectElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLSelectElement.Set_selectedIndex(AValue : LongInt); //property selectedIndex : LongInt read Get_selectedIndex write Set_selectedIndex;
begin
  SetPropertyValue('selectedIndex', AValue);
end;

procedure TjsHTMLSelectElement.Set_size(AValue : LongInt); //property size : LongInt read Get_size write Set_size;
begin
  SetPropertyValue('size', AValue);
end;

procedure TjsHTMLSelectElement.Set_tabIndex(AValue : LongInt); //property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
begin
  SetPropertyValue('tabIndex', AValue);
end;

procedure TjsHTMLSelectElement.Set_value(AValue : DOMString); //property value : DOMString read Get_value write Set_value;
begin
  SetPropertyValue('value', AValue);
end;

{ TjsHTMLBodyElement }

function TjsHTMLBodyElement.Get_aLink : DOMString;// procedure Set_aLink(AValue : DOMString);
begin
  GetpropertyValue('aLink', Result)
end;

function TjsHTMLBodyElement.Get_background : DOMString;// procedure Set_background(AValue : DOMString);
begin
  GetpropertyValue('background', Result)
end;

function TjsHTMLBodyElement.Get_bgColor : DOMString;// procedure Set_bgColor(AValue : DOMString);
begin
  GetpropertyValue('bgColor', Result)
end;

function TjsHTMLBodyElement.Get_link : DOMString;// procedure Set_link(AValue : DOMString);
begin
  GetpropertyValue('link', Result)
end;

function TjsHTMLBodyElement.Get_text : DOMString;// procedure Set_text(AValue : DOMString);
begin
  GetpropertyValue('text', Result)
end;

function TjsHTMLBodyElement.Get_vLink : DOMString;// procedure Set_vLink(AValue : DOMString);
begin
  GetpropertyValue('vLink', Result)
end;

procedure TjsHTMLBodyElement.Set_aLink(AValue : DOMString); //property aLink : DOMString read Get_aLink write Set_aLink;
begin
  SetPropertyValue('aLink', AValue);
end;

procedure TjsHTMLBodyElement.Set_background(AValue : DOMString); //property background : DOMString read Get_background write Set_background;
begin
  SetPropertyValue('background', AValue);
end;

procedure TjsHTMLBodyElement.Set_bgColor(AValue : DOMString); //property bgColor : DOMString read Get_bgColor write Set_bgColor;
begin
  SetPropertyValue('bgColor', AValue);
end;

procedure TjsHTMLBodyElement.Set_link(AValue : DOMString); //property link : DOMString read Get_link write Set_link;
begin
  SetPropertyValue('link', AValue);
end;

procedure TjsHTMLBodyElement.Set_text(AValue : DOMString); //property text : DOMString read Get_text write Set_text;
begin
  SetPropertyValue('text', AValue);
end;

procedure TjsHTMLBodyElement.Set_vLink(AValue : DOMString); //property vLink : DOMString read Get_vLink write Set_vLink;
begin
  SetPropertyValue('vLink', AValue);
end;

{ TjsHTMLStyleElement }

function TjsHTMLStyleElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLStyleElement.Get_media : DOMString;// procedure Set_media(AValue : DOMString);
begin
  GetpropertyValue('media', Result)
end;

function TjsHTMLStyleElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

procedure TjsHTMLStyleElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLStyleElement.Set_media(AValue : DOMString); //property media : DOMString read Get_media write Set_media;
begin
  SetPropertyValue('media', AValue);
end;

procedure TjsHTMLStyleElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

{ TjsHTMLFormElement }

function TjsHTMLFormElement.Get_acceptCharset : DOMString;// procedure Set_acceptCharset(AValue : DOMString);
begin
  GetpropertyValue('acceptCharset', Result)
end;

function TjsHTMLFormElement.Get_action : DOMString;// procedure Set_action(AValue : DOMString);
begin
  GetpropertyValue('action', Result)
end;

function TjsHTMLFormElement.Get_elements : IHTMLCollection; //property elements : IHTMLCollection read Get_elements;
begin
  GetPropertyValueIntf('elements', Result, TjsHTMLCollection)
end;

function TjsHTMLFormElement.Get_enctype : DOMString;// procedure Set_enctype(AValue : DOMString);
begin
  GetpropertyValue('enctype', Result)
end;

function TjsHTMLFormElement.Get_form : IHTMLFormElement; //property form : IHTMLFormElement read Get_form;
begin
  GetPropertyValueIntf('form', Result, TjsHTMLFormElement)
end;

function TjsHTMLFormElement.Get_length : LongInt; //property length : longint read Get_length;
begin
  GetpropertyValue('length', Result)
end;

function TjsHTMLFormElement.Get_method : DOMString;// procedure Set_method(AValue : DOMString);
begin
  GetpropertyValue('method', Result)
end;

function TjsHTMLFormElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLFormElement.Get_prompt : DOMString;// procedure Set_prompt(AValue : DOMString);
begin
  GetpropertyValue('prompt', Result)
end;

function TjsHTMLFormElement.Get_target : DOMString;// procedure Set_target(AValue : DOMString);
begin
  GetpropertyValue('target', Result)
end;

procedure TjsHTMLFormElement.reset;
begin
  ExecMethod('reset');
end;

procedure TjsHTMLFormElement.Set_acceptCharset(AValue : DOMString); //property acceptCharset : DOMString read Get_acceptCharset write Set_acceptCharset;
begin
  SetPropertyValue('acceptCharset', AValue);
end;

procedure TjsHTMLFormElement.Set_action(AValue : DOMString); //property action : DOMString read Get_action write Set_action;
begin
  SetPropertyValue('action', AValue);
end;

procedure TjsHTMLFormElement.Set_enctype(AValue : DOMString); //property enctype : DOMString read Get_enctype write Set_enctype;
begin
  SetPropertyValue('enctype', AValue);
end;

procedure TjsHTMLFormElement.Set_method(AValue : DOMString); //property method : DOMString read Get_method write Set_method;
begin
  SetPropertyValue('method', AValue);
end;

procedure TjsHTMLFormElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLFormElement.Set_prompt(AValue : DOMString); //property prompt : DOMString read Get_prompt write Set_prompt;
begin
  SetPropertyValue('prompt', AValue);
end;

procedure TjsHTMLFormElement.Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
begin
  SetPropertyValue('target', AValue);
end;

procedure TjsHTMLFormElement.submit;
begin
  ExecMethod('submit')
end;

{ TjsHTMLBaseElement }

function TjsHTMLBaseElement.Get_href : DOMString;// procedure Set_href(AValue : DOMString);
begin
  GetpropertyValue('href', Result)
end;

function TjsHTMLBaseElement.Get_target : DOMString;// procedure Set_target(AValue : DOMString);
begin
  GetpropertyValue('target', Result)
end;

procedure TjsHTMLBaseElement.Set_href(AValue : DOMString); //property href : DOMString read Get_href write Set_href;
begin
  SetPropertyValue('href', AValue);
end;

procedure TjsHTMLBaseElement.Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
begin
  SetPropertyValue('target', AValue);
end;

{ TjsHTMLMetaElement }

function TjsHTMLMetaElement.Get_content : DOMString;// procedure Set_content(AValue : DOMString);
begin
  GetpropertyValue('content', Result)
end;

function TjsHTMLMetaElement.Get_httpEquiv : DOMString;// procedure Set_httpEquiv(AValue : DOMString);
begin
  GetpropertyValue('httpEquiv', Result)
end;

function TjsHTMLMetaElement.Get_name : DOMString;// procedure Set_name(AValue : DOMString);
begin
  GetpropertyValue('name', Result)
end;

function TjsHTMLMetaElement.Get_scheme : DOMString;// procedure Set_scheme(AValue : DOMString);
begin
  GetpropertyValue('scheme', Result)
end;

procedure TjsHTMLMetaElement.Set_content(AValue : DOMString); //property content : DOMString read Get_content write Set_content;
begin
  SetPropertyValue('content', AValue);
end;

procedure TjsHTMLMetaElement.Set_httpEquiv(AValue : DOMString); //property httpEquiv : DOMString read Get_httpEquiv write Set_httpEquiv;
begin
  SetPropertyValue('httpEquiv', AValue);
end;

procedure TjsHTMLMetaElement.Set_name(AValue : DOMString); //property name : DOMString read Get_name write Set_name;
begin
  SetPropertyValue('name', AValue);
end;

procedure TjsHTMLMetaElement.Set_scheme(AValue : DOMString); //property scheme : DOMString read Get_scheme write Set_scheme;
begin
  SetPropertyValue('scheme', AValue);
end;

{ TjsHTMLTitleElement }

function TjsHTMLTitleElement.Get_text : DOMString;// procedure Set_text(AValue : DOMString);
begin
  GetpropertyValue('text', Result)
end;

procedure TjsHTMLTitleElement.Set_text(AValue : DOMString); //property text : DOMString read Get_text write Set_text;
begin
  SetPropertyValue('text', AValue);
end;

{ TjsHTMLLinkElement }

function TjsHTMLLinkElement.Get_charset : DOMString;// procedure Set_charset(AValue : DOMString);
begin
  GetpropertyValue('charset', Result)
end;

function TjsHTMLLinkElement.Get_disabled : Boolean;// procedure Set_disabled(AValue : Boolean);
begin
  GetpropertyValue('disabled', Result)
end;

function TjsHTMLLinkElement.Get_href : DOMString;// procedure Set_href(AValue : DOMString);
begin
  GetpropertyValue('href', Result)
end;

function TjsHTMLLinkElement.Get_hreflang : DOMString;// procedure Set_hreflang(AValue : DOMString);
begin
  GetpropertyValue('hreflang', Result)
end;

function TjsHTMLLinkElement.Get_media : DOMString;// procedure Set_media(AValue : DOMString);
begin
  GetpropertyValue('media', Result)
end;

function TjsHTMLLinkElement.Get_rel : DOMString;// procedure Set_rel(AValue : DOMString);
begin
  GetpropertyValue('rel', Result)
end;

function TjsHTMLLinkElement.Get_rev : DOMString;// procedure Set_rev(AValue : DOMString);
begin
  GetpropertyValue('rev', Result)
end;

function TjsHTMLLinkElement.Get_target : DOMString;// procedure Set_target(AValue : DOMString);
begin
  GetpropertyValue('target', Result)
end;

function TjsHTMLLinkElement.Get_type : DOMString;// procedure Set_type(AValue : DOMString);
begin
  GetpropertyValue('type', Result)
end;

procedure TjsHTMLLinkElement.Set_charset(AValue : DOMString); //property charset : DOMString read Get_charset write Set_charset;
begin
  SetPropertyValue('charset', AValue);
end;

procedure TjsHTMLLinkElement.Set_disabled(AValue : Boolean); //property disabled : Boolean read Get_disabled write Set_disabled;
begin
  SetPropertyValue('disabled', AValue);
end;

procedure TjsHTMLLinkElement.Set_href(AValue : DOMString); //property href : DOMString read Get_href write Set_href;
begin
  SetPropertyValue('href', AValue);
end;

procedure TjsHTMLLinkElement.Set_hreflang(AValue : DOMString); //property hreflang : DOMString read Get_hreflang write Set_hreflang;
begin
  SetPropertyValue('hreflang', AValue);
end;

procedure TjsHTMLLinkElement.Set_media(AValue : DOMString); //property media : DOMString read Get_media write Set_media;
begin
  SetPropertyValue('media', AValue);
end;

procedure TjsHTMLLinkElement.Set_rel(AValue : DOMString); //property rel : DOMString read Get_rel write Set_rel;
begin
  SetPropertyValue('rel', AValue);
end;

procedure TjsHTMLLinkElement.Set_rev(AValue : DOMString); //property rev : DOMString read Get_rev write Set_rev;
begin
  SetPropertyValue('rev', AValue);
end;

procedure TjsHTMLLinkElement.Set_target(AValue : DOMString); //property target : DOMString read Get_target write Set_target;
begin
  SetPropertyValue('target', AValue);
end;

procedure TjsHTMLLinkElement.Set_type(AValue : DOMString); //property _type : DOMString read Get_type write Set_type;
begin
  SetPropertyValue('type', AValue);
end;

{ TjsHTMLHeadElement }

function TjsHTMLHeadElement.Get_profile : DOMString;// procedure Set_profile(AValue : DOMString);
begin
  GetpropertyValue('profile', Result)
end;

procedure TjsHTMLHeadElement.Set_profile(AValue : DOMString); //property profile : DOMString read Get_profile write Set_profile;
begin
  SetPropertyValue('profile', AValue);
end;

{ TjsHTMLHtmlElement }

function TjsHTMLHtmlElement.Get_version : DOMString;// procedure Set_version(AValue : DOMString);
begin
  GetpropertyValue('version', Result)
end;

procedure TjsHTMLHtmlElement.Set_version(AValue : DOMString); //property version : DOMString read Get_version write Set_version;
begin
  SetPropertyValue('version', AValue);
end;

{ TjsHTMLDocument }

procedure TjsHTMLDocument.close;
begin
  ExecMethod('close');
end;

function TjsHTMLDocument.getElementsByName(
  elementName: DOMString): IDOMNodeList;
begin
  ExecMethod('getElementsByName('+ToJSCodeEx(elementName)+')', TjsDOMNodeList);
end;

function TjsHTMLDocument.Get_anchors : IHTMLCollection; //property anchors : IHTMLCollection read Get_anchors;
begin
  GetPropertyValueIntf('anchors', Result, TjsHTMLCollection)
end;

function TjsHTMLDocument.Get_applets : IHTMLCollection; //property applets : IHTMLCollection read Get_applets;
begin
  GetPropertyValueIntf('applets', Result, TjsHTMLCollection)
end;

function TjsHTMLDocument.Get_body : IHTMLElement;// procedure Set_body(AValue : IHTMLElement);
begin
  GetPropertyValueIntf('body', Result, TjsHTMLBodyElement)
end;

function TjsHTMLDocument.Get_cookie : DOMString;// procedure Set_cookie(AValue : DOMString);
begin
  GetPropertyValue('cookie', Result)
end;

function TjsHTMLDocument.Get_domain : DOMString; //property domain : DOMString read Get_domain;
begin
  GetPropertyValue('domain', Result)
end;

function TjsHTMLDocument.Get_forms : IHTMLCollection; //property forms : IHTMLCollection read Get_forms;
begin
  GetPropertyValueIntf('forms', Result, TjsHTMLCollection)
end;

function TjsHTMLDocument.Get_images : IHTMLCollection; //property images : IHTMLCollection read Get_images;
begin
  GetPropertyValueIntf('images', Result, TjsHTMLCollection)
end;

function TjsHTMLDocument.Get_links : IHTMLCollection; //property links : IHTMLCollection read Get_links;
begin
  GetPropertyValueIntf('links', Result, TjsHTMLCollection)
end;

function TjsHTMLDocument.Get_referrer : DOMString; //property referrer : DOMString read Get_referrer;
begin
  GetpropertyValue('referrer', Result)
end;

function TjsHTMLDocument.Get_title : DOMString;// procedure Set_title(AValue : DOMString);
begin
  GetpropertyValue('title', Result)
end;

function TjsHTMLDocument.Get_URL : DOMString; //property URL : DOMString read Get_URL;
begin
  GetpropertyValue('URL', Result)
end;

procedure TjsHTMLDocument.open;
begin

end;

procedure TjsHTMLDocument.Set_body(AValue : IHTMLElement); //property body : IHTMLElement read Get_body write Set_body;
begin
  SetPropertyValue('body', AValue);
end;

procedure TjsHTMLDocument.Set_cookie(AValue : DOMString); //property cookie : DOMString read Get_cookie write Set_cookie;
begin
  SetPropertyValue('cookie', AValue);
end;

procedure TjsHTMLDocument.Set_title(AValue : DOMString); //property title : DOMString read Get_title write Set_title;
begin
  SetPropertyValue('title', AValue);
end;

procedure TjsHTMLDocument.write(text : DOMString);// procedure writeln(text : DOMString);
begin
  ExecMethod('write('+ToJSCodeEx(text)+')');
end;

procedure TjsHTMLDocument.writeln(text: DOMString);
begin
  ExecMethod('writeln('+ToJSCodeEx(text)+')');
end;

{ TjsHTMLOptionsCollection }

function TjsHTMLOptionsCollection.item(index : LongInt) : IDOMNode;
begin
  Result:=ExecMethod('item('+ToJSCodeEx(index)+')', TjsDOMNode) as IDOMNode;
end;

function TjsHTMLOptionsCollection.namedItem(name: DOMString): IDOMNode;
begin
  Result:=ExecMethod('namedItem('+ToJSCodeEx(name)+')', TjsDOMNode) as IDOMNode;
end;

{ TjsHTMLElement }

function TjsHTMLElement.Get_className : DOMString;
begin
 // GetPropertyValue('className', Result)
end;

function TjsHTMLElement.Get_dir : DOMString;// procedure Set_dir(AValue : DOMString);
begin
  GetpropertyValue('dir', Result)
end;

function TjsHTMLElement.Get_id : DOMString;// procedure Set_id(AValue : DOMString);
begin
  GetpropertyValue('id', Result)
end;

function TjsHTMLElement.Get_lang : DOMString;// procedure Set_lang(AValue : DOMString);
begin
  GetpropertyValue('lang', Result)
end;

function TjsHTMLElement.Get_title : DOMString;// procedure Set_title(AValue : DOMString);
begin
  GetpropertyValue('title', Result)
end;

function TjsHTMLElement.item(index : LongInt) : IDOMNode;
begin
  Result:=ExecMethod('item('+ToJSCodeEx(index)+')', TjsDOMNode) as IDOMNode;
end;

function TjsHTMLElement.namedItem(name: DOMString): IDOMNode;
begin
  Result:=ExecMethod('namedItem('+ToJSCodeEx(name)+')', TjsDOMNode) as IDOMNode;
end;

procedure TjsHTMLElement.Set_className(AValue : DOMString); //property className : DOMString read Get_className write Set_className;
begin
  SetPropertyValue('className', AValue);
end;

procedure TjsHTMLElement.Set_dir(AValue : DOMString); //property dir : DOMString read Get_dir write Set_dir;
begin
  SetPropertyValue('dir', AValue);
end;

procedure TjsHTMLElement.Set_id(AValue : DOMString); //property id : DOMString read Get_id write Set_id;
begin
  SetPropertyValue('id', AValue);
end;

procedure TjsHTMLElement.Set_lang(AValue : DOMString); //property lang : DOMString read Get_lang write Set_lang;
begin
  SetPropertyValue('lang', AValue);
end;

procedure TjsHTMLElement.Set_title(AValue : DOMString); //property title : DOMString read Get_title write Set_title;
begin
  SetPropertyValue('title', AValue);
end;

{ TjsHTMLCollection }

function TjsHTMLCollection.item(index: Integer): IDOMNode;
begin
  result:=ExecMethod('item('+ToJSCodeEx(index)+')', TjsDOMNode) as IDOMNode;
end;

function TjsHTMLCollection.namedItem(name: DOMString): IDOMNode;
begin
  result:=ExecMethod('namedItem('+ToJSCodeEx(name)+')', TjsDOMNode) as IDOMNOde;
end;

end.
