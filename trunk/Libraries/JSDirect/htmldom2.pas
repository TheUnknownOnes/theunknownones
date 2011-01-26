unit htmldom2;

interface

uses
  xmldom, windows;

type
  IHTMLElement = interface;

  IHTMLCollection = interface
  ['{39F65367-2530-4E40-9285-8D87D41DE3FB}']
    function item(index : LongInt) : IDOMNode;
    function namedItem(name : DOMString) : IDOMNode;
  end;

  IHTMLOptionsCollection = interface
  ['{3F9D3563-B96F-43EC-9834-D5AB78F512A4}']
    function item(index : LongInt) : IDOMNode;
    function namedItem(name : DOMString) : IDOMNode;
  end;

  IHTMLDocument = interface(IDOMDocument)
  ['{3612D637-1603-4FF4-9D1B-CCB2776703FA}']
    function Get_referrer : DOMString;
    property referrer : DOMString read Get_referrer;
    function Get_domain : DOMString;
    property domain : DOMString read Get_domain;
    function Get_URL : DOMString;
    property URL : DOMString read Get_URL;
    function Get_images : IHTMLCollection;
    property images : IHTMLCollection read Get_images;
    function Get_applets : IHTMLCollection;
  property applets : IHTMLCollection read Get_applets;
    function Get_links : IHTMLCollection;
  property links : IHTMLCollection read Get_links;
    function Get_forms : IHTMLCollection;
  property forms : IHTMLCollection read Get_forms;
    function Get_anchors : IHTMLCollection;
  property anchors : IHTMLCollection read Get_anchors;
    function Get_title : DOMString;
    procedure Set_title(AValue : DOMString);
    property title : DOMString read Get_title write Set_title;
    function Get_body : IHTMLElement;
    procedure Set_body(AValue : IHTMLElement);
    property body : IHTMLElement read Get_body write Set_body;
    function Get_cookie : DOMString;
    procedure Set_cookie(AValue : DOMString);
    property cookie : DOMString read Get_cookie write Set_cookie;
    procedure open();
    procedure close();
    procedure write(text : DOMString);
    procedure writeln(text : DOMString);
    function getElementsByName(elementName : DOMString) : IDOMNodeList;
  end;

  IHTMLElement = interface(IDOMElement)
  ['{9A3561DC-8D77-4170-BCAE-66B2C561E00A}']
    function Get_id : DOMString;
    procedure Set_id(AValue : DOMString);
    property id : DOMString read Get_id write Set_id;
    function Get_title : DOMString;
    procedure Set_title(AValue : DOMString);
    property title : DOMString read Get_title write Set_title;
    function Get_lang : DOMString;
    procedure Set_lang(AValue : DOMString);
    property lang : DOMString read Get_lang write Set_lang;
    function Get_dir : DOMString;
    procedure Set_dir(AValue : DOMString);
    property dir : DOMString read Get_dir write Set_dir;
    function Get_className : DOMString;
    procedure Set_className(AValue : DOMString);
    property className : DOMString read Get_className write Set_className;
  end;

  IHTMLHtmlElement = interface(IHTMLElement)
  ['{56C72746-E014-4166-89C5-90C5E2401B3D}']
    function Get_version : DOMString;
    procedure Set_version(AValue : DOMString);
    property version : DOMString read Get_version write Set_version;
  end;

  IHTMLHeadElement = interface(IHTMLElement)
  ['{37922A87-E379-454B-9525-D1BB05DD9E43}']
    function Get_profile : DOMString;
    procedure Set_profile(AValue : DOMString);
    property profile : DOMString read Get_profile write Set_profile;
  end;

  IHTMLLinkElement = interface(IHTMLElement)
  ['{88081C0D-4C50-4EA1-8D65-592062D6FA9F}']
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_charset : DOMString;
    procedure Set_charset(AValue : DOMString);
    property charset : DOMString read Get_charset write Set_charset;
    function Get_href : DOMString;
    procedure Set_href(AValue : DOMString);
    property href : DOMString read Get_href write Set_href;
    function Get_hreflang : DOMString;
    procedure Set_hreflang(AValue : DOMString);
    property hreflang : DOMString read Get_hreflang write Set_hreflang;
    function Get_media : DOMString;
    procedure Set_media(AValue : DOMString);
    property media : DOMString read Get_media write Set_media;
    function Get_rel : DOMString;
    procedure Set_rel(AValue : DOMString);
    property rel : DOMString read Get_rel write Set_rel;
    function Get_rev : DOMString;
    procedure Set_rev(AValue : DOMString);
    property rev : DOMString read Get_rev write Set_rev;
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
    property target : DOMString read Get_target write Set_target;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
  end;

  IHTMLTitleElement = interface(IHTMLElement)
  ['{95901A42-2DEF-4898-9009-641D8CB85DBD}']
    function Get_text : DOMString;
    procedure Set_text(AValue : DOMString);
    property text : DOMString read Get_text write Set_text;
  end;

  IHTMLMetaElement = interface(IHTMLElement)
  ['{04E8965F-8FEB-4059-9D3B-9B805AC565EA}']
    function Get_content : DOMString;
    procedure Set_content(AValue : DOMString);
    property content : DOMString read Get_content write Set_content;
    function Get_httpEquiv : DOMString;
    procedure Set_httpEquiv(AValue : DOMString);
    property httpEquiv : DOMString read Get_httpEquiv write Set_httpEquiv;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_scheme : DOMString;
    procedure Set_scheme(AValue : DOMString);
    property scheme : DOMString read Get_scheme write Set_scheme;
  end;

  IHTMLBaseElement = interface(IHTMLElement)
  ['{0BFF53A6-7795-432F-9B57-E29EB8054E4E}']
    function Get_href : DOMString;
    procedure Set_href(AValue : DOMString);
    property href : DOMString read Get_href write Set_href;
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
    property target : DOMString read Get_target write Set_target;
  end;

  IHTMLFormElement = interface;

  IHTMLIsIndexElement = interface(IHTMLElement)
  ['{BE210784-219F-4168-B458-8377D3888C20}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_prompt : DOMString;
    procedure Set_prompt(AValue : DOMString);
    property prompt : DOMString read Get_prompt write Set_prompt;
  end;

  IHTMLStyleElement = interface(IHTMLElement)
  ['{4D0B69C8-59F5-4698-8218-B2C7FC25227C}']
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_media : DOMString;
    procedure Set_media(AValue : DOMString);
    property media : DOMString read Get_media write Set_media;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
  end;

  IHTMLBodyElement = interface(IHTMLElement)
  ['{6F307E06-7147-4A94-982B-F6452504C6CD}']
    function Get_aLink : DOMString;
    procedure Set_aLink(AValue : DOMString);
    property aLink : DOMString read Get_aLink write Set_aLink;
    function Get_background : DOMString;
    procedure Set_background(AValue : DOMString);
    property background : DOMString read Get_background write Set_background;
    function Get_bgColor : DOMString;
    procedure Set_bgColor(AValue : DOMString);
    property bgColor : DOMString read Get_bgColor write Set_bgColor;
    function Get_link : DOMString;
    procedure Set_link(AValue : DOMString);
    property link : DOMString read Get_link write Set_link;
    function Get_text : DOMString;
    procedure Set_text(AValue : DOMString);
    property text : DOMString read Get_text write Set_text;
    function Get_vLink : DOMString;
    procedure Set_vLink(AValue : DOMString);
    property vLink : DOMString read Get_vLink write Set_vLink;
  end;

  IHTMLFormElement = interface(IHTMLElement)
  ['{213739B6-034E-443A-B4A1-7E796F9CD777}']
    function Get_elements : IHTMLCollection;
    property elements : IHTMLCollection read Get_elements;
    function Get_length : LongInt;
    property length : longint read Get_length;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_acceptCharset : DOMString;
    procedure Set_acceptCharset(AValue : DOMString);
    property acceptCharset : DOMString read Get_acceptCharset write Set_acceptCharset;
    function Get_action : DOMString;
    procedure Set_action(AValue : DOMString);
    property action : DOMString read Get_action write Set_action;
    function Get_enctype : DOMString;
    procedure Set_enctype(AValue : DOMString);
    property enctype : DOMString read Get_enctype write Set_enctype;
    function Get_method : DOMString;
    procedure Set_method(AValue : DOMString);
    property method : DOMString read Get_method write Set_method;
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
    property target : DOMString read Get_target write Set_target;
    procedure submit();
    procedure reset();
  end;

  IHTMLSelectElement = interface(IHTMLElement)
  ['{8DC2615C-1260-48D6-ADB6-8C9D88A148FD}']
    function Get_type : DOMString;
    property _type : DOMString read Get_type;
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_options : IHTMLOptionsCollection;
    property options : IHTMLOptionsCollection read Get_options;
    function Get_selectedIndex : LongInt;
    procedure Set_selectedIndex(AValue : LongInt);
    property selectedIndex : LongInt read Get_selectedIndex write Set_selectedIndex;
    function Get_value : DOMString;
    procedure Set_value(AValue : DOMString);
    property value : DOMString read Get_value write Set_value;
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_multiple : Boolean;
    procedure Set_multiple(AValue : Boolean);
    property multiple : Boolean read Get_multiple write Set_multiple;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_size : LongInt;
    procedure Set_size(AValue : LongInt);
    property size : LongInt read Get_size write Set_size;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    procedure add(element : IHTMLElement; before : IHTMLElement);
    procedure remove(index : LongInt);
    procedure blur();
    procedure focus();
  end;

  IHTMLOptGroupElement = interface(IHTMLElement)
  ['{0F2A2489-BE4E-417F-9EFE-ABA7C877CBA1}']
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_label : DOMString;
    procedure Set_label(AValue : DOMString);
    property _label : DOMString read Get_label write Set_label;
  end;

  IHTMLOptionElement = interface(IHTMLElement)
  ['{BA076180-91F6-46A2-9738-1906666532D5}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_text : DOMString;
    property text : DOMString read Get_text;
    function Get_index : LongInt;
    property _index : longInt read Get_index;
    function Get_defaultSelected : Boolean;
    procedure Set_defaultSelected(AValue : Boolean);
    property defaultSelected : Boolean read Get_defaultSelected write Set_defaultSelected;
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_label : DOMString;
    procedure Set_label(AValue : DOMString);
    property _label : DOMString read Get_label write Set_label;
    function Get_selected : Boolean;
    procedure Set_selected(AValue : Boolean);
    property selected : Boolean read Get_selected write Set_selected;
    function Get_value : DOMString;
    procedure Set_value(AValue : DOMString);
    property value : DOMString read Get_value write Set_value;
  end;

  IHTMLInputElement = interface(IHTMLElement)
  ['{8E734254-5948-4C2E-8B7F-24EBE64AFA06}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_defaultValue : DOMString;
    procedure Set_defaultValue(AValue : DOMString);
    property defaultValue : DOMString read Get_defaultValue write Set_defaultValue;
    function Get_defaultChecked : Boolean;
    procedure Set_defaultChecked(AValue : Boolean);
    property defaultChecked : Boolean read Get_defaultChecked write Set_defaultChecked;
    function Get_accept : DOMString;
    procedure Set_accept(AValue : DOMString);
    property accept : DOMString read Get_accept write Set_accept;
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_alt : DOMString;
    procedure Set_alt(AValue : DOMString);
    property alt : DOMString read Get_alt write Set_alt;
    function Get_checked : Boolean;
    procedure Set_checked(AValue : Boolean);
    property checked : Boolean read Get_checked write Set_checked;
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_maxLength : LongInt;
    procedure Set_maxLength(AValue : LongInt);
    property maxLength : LongInt read Get_maxLength write Set_maxLength;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_readOnly : Boolean;
    procedure Set_readOnly(AValue : Boolean);
    property _readOnly : Boolean read Get_readOnly write Set_readOnly;
    function Get_src : DOMString;
    procedure Set_src(AValue : DOMString);
    property src : DOMString read Get_src write Set_src;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
    function Get_useMap : DOMString;
    procedure Set_useMap(AValue : DOMString);
    property useMap : DOMString read Get_useMap write Set_useMap;
    function Get_value : DOMString;
    procedure Set_value(AValue : DOMString);
    property value : DOMString read Get_value write Set_value;
    procedure blur();
    procedure focus();
    procedure select();
    procedure click();
  end;

  IHTMLTextAreaElement = interface(IHTMLElement)
  ['{FC66F286-B529-418C-8296-421698D79331}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_type : DOMString;
    property _type : DOMString read Get_type;
    function Get_defaultValue : DOMString;
    procedure Set_defaultValue(AValue : DOMString);
    property defaultValue : DOMString read Get_defaultValue write Set_defaultValue;
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_cols : LongInt;
    procedure Set_cols(AValue : LongInt);
    property cols : LongInt read Get_cols write Set_cols;
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_readOnly : Boolean;
    procedure Set_readOnly(AValue : Boolean);
    property _readOnly : Boolean read Get_readOnly write Set_readOnly;
    function Get_rows : LongInt;
    procedure Set_rows(AValue : LongInt);
    property rows : LongInt read Get_rows write Set_rows;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    function Get_value : DOMString;
    procedure Set_value(AValue : DOMString);
    property value : DOMString read Get_value write Set_value;
    procedure blur();
    procedure focus();
    procedure select();
  end;

  IHTMLButtonElement = interface(IHTMLElement)
  ['{2ED99427-3132-4B7D-B461-9C510EB6BF11}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_type : DOMString;
    property _type : DOMString read Get_type;
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_disabled : Boolean;
    procedure Set_disabled(AValue : Boolean);
    property disabled : Boolean read Get_disabled write Set_disabled;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    function Get_value : DOMString;
    procedure Set_value(AValue : DOMString);
    property value : DOMString read Get_value write Set_value;
  end;

  IHTMLLabelElement = interface(IHTMLElement)
  ['{F43CF1D2-A178-46DC-8020-B072110ED147}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_htmlFor : DOMString;
    procedure Set_htmlFor(AValue : DOMString);
    property htmlFor : DOMString read Get_htmlFor write Set_htmlFor;
  end;

  IHTMLFieldSetElement = interface(IHTMLElement)
  ['{9D41BF54-197D-4DFE-9F61-E75428DAA46C}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
  end;

  IHTMLLegendElement = interface(IHTMLElement)
  ['{C32AB582-9876-4523-B834-5FC1FD809972}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
  end;

  IHTMLUListElement = interface(IHTMLElement)
  ['{2C6ACE67-7C8A-49F4-A909-AF431D816D82}']
    function Get_compact : Boolean;
    procedure Set_compact(AValue : Boolean);
    property compact : Boolean read Get_compact write Set_compact;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
  end;

  IHTMLOListElement = interface(IHTMLElement)
  ['{3E73CA05-3F8E-40A5-944F-726A95135170}']
    function Get_compact : Boolean;
    procedure Set_compact(AValue : Boolean);
    property compact : Boolean read Get_compact write Set_compact;
    function Get_start : LongInt;
    procedure Set_start(AValue : LongInt);
    property start : LongInt read Get_start write Set_start;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
  end;

  IHTMLDListElement = interface(IHTMLElement)
  ['{A8C1448C-95F3-4785-BBCA-E7E74E7DED08}']
    function Get_compact : Boolean;
    procedure Set_compact(AValue : Boolean);
    property compact : Boolean read Get_compact write Set_compact;
  end;

  IHTMLDirectoryElement = interface(IHTMLElement)
  ['{64F2F0CE-C53F-4110-B8F2-9324F6D82EB6}']
    function Get_compact : Boolean;
    procedure Set_compact(AValue : Boolean);
    property compact : Boolean read Get_compact write Set_compact;
  end;

  IHTMLMenuElement = interface(IHTMLElement)
  ['{24EE2182-6E10-4E99-9851-12D4AE374436}']
    function Get_compact : Boolean;
    procedure Set_compact(AValue : Boolean);
    property compact : Boolean read Get_compact write Set_compact;
  end;

  IHTMLLIElement = interface(IHTMLElement)
  ['{513967D5-FDA1-4584-BF80-86DD441F10A5}']
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
    function Get_value : LongInt;
    procedure Set_value(AValue : LongInt);
    property value : LongInt read Get_value write Set_value;
  end;

  IHTMLDivElement = interface(IHTMLElement)
  ['{CD9345AE-0028-403C-BADD-174D11490F96}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
  end;

  IHTMLParagraphElement = interface(IHTMLElement)
  ['{99BAF967-8C78-4870-9FD2-A2BE1651EA31}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
  end;

  IHTMLHeadingElement = interface(IHTMLElement)
  ['{E7E061DA-D826-439F-BE1B-F66B4F24A2FC}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
  end;

  IHTMLQuoteElement = interface(IHTMLElement)
  ['{ACA1642A-4A7B-4361-93EF-7F36AACDDC2A}']
    function Get_cite : DOMString;
    procedure Set_cite(AValue : DOMString);
    property cite : DOMString read Get_cite write Set_cite;
  end;

  IHTMLPreElement = interface(IHTMLElement)
  ['{3BF40547-5713-4D3C-ADBE-6E970ADBA813}']
    function Get_width : LongInt;
    procedure Set_width(AValue : LongInt);
    property width : LongInt read Get_width write Set_width;
  end;

  IHTMLBRElement = interface(IHTMLElement)
  ['{28616E3D-84F7-43B7-BD0D-84325B64C24D}']
    function Get_clear : DOMString;
    procedure Set_clear(AValue : DOMString);
    property clear : DOMString read Get_clear write Set_clear;
  end;

  IHTMLBaseFontElement = interface(IHTMLElement)
  ['{76D7D579-CEDB-43FC-84B1-2155E765E933}']
    function Get_color : DOMString;
    procedure Set_color(AValue : DOMString);
    property color : DOMString read Get_color write Set_color;
    function Get_face : DOMString;
    procedure Set_face(AValue : DOMString);
    property face : DOMString read Get_face write Set_face;
    function Get_size : LongInt;
    procedure Set_size(AValue : LongInt);
    property size : LongInt read Get_size write Set_size;
  end;

  IHTMLFontElement = interface(IHTMLElement)
  ['{644899D6-F328-48CC-8C38-593DE8D72546}']
    function Get_color : DOMString;
    procedure Set_color(AValue : DOMString);
    property color : DOMString read Get_color write Set_color;
    function Get_face : DOMString;
    procedure Set_face(AValue : DOMString);
    property face : DOMString read Get_face write Set_face;
    function Get_size : DOMString;
    procedure Set_size(AValue : DOMString);
    property size : DOMString read Get_size write Set_size;
  end;

  IHTMLHRElement = interface(IHTMLElement)
  ['{6B8F3BF0-1E25-45F0-8736-E88988492475}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_noShade : Boolean;
    procedure Set_noShade(AValue : Boolean);
    property noShade : Boolean read Get_noShade write Set_noShade;
    function Get_size : DOMString;
    procedure Set_size(AValue : DOMString);
    property size : DOMString read Get_size write Set_size;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
  end;

  IHTMLModElement = interface(IHTMLElement)
  ['{8E5FB40F-FB8C-467C-B96F-8D4E76C5EB86}']
    function Get_cite : DOMString;
    procedure Set_cite(AValue : DOMString);
    property cite : DOMString read Get_cite write Set_cite;
    function Get_dateTime : DOMString;
    procedure Set_dateTime(AValue : DOMString);
    property dateTime : DOMString read Get_dateTime write Set_dateTime;
  end;

  IHTMLAnchorElement = interface(IHTMLElement)
  ['{71A5E50F-133A-481D-ADB6-F7B590B0D30E}']
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_charset : DOMString;
    procedure Set_charset(AValue : DOMString);
    property charset : DOMString read Get_charset write Set_charset;
    function Get_coords : DOMString;
    procedure Set_coords(AValue : DOMString);
    property coords : DOMString read Get_coords write Set_coords;
    function Get_href : DOMString;
    procedure Set_href(AValue : DOMString);
    property href : DOMString read Get_href write Set_href;
    function Get_hreflang : DOMString;
    procedure Set_hreflang(AValue : DOMString);
    property hreflang : DOMString read Get_hreflang write Set_hreflang;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_rel : DOMString;
    procedure Set_rel(AValue : DOMString);
    property rel : DOMString read Get_rel write Set_rel;
    function Get_rev : DOMString;
    procedure Set_rev(AValue : DOMString);
    property rev : DOMString read Get_rev write Set_rev;
    function Get_shape : DOMString;
    procedure Set_shape(AValue : DOMString);
    property shape : DOMString read Get_shape write Set_shape;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
    property target : DOMString read Get_target write Set_target;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
    procedure blur();
    procedure focus();
  end;

  IHTMLImageElement = interface(IHTMLElement)
  ['{77CCA60B-67A2-4660-867B-4E513D6239D2}']
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_alt : DOMString;
    procedure Set_alt(AValue : DOMString);
    property alt : DOMString read Get_alt write Set_alt;
    function Get_border : DOMString;
    procedure Set_border(AValue : DOMString);
    property border : DOMString read Get_border write Set_border;
    function Get_height : LongInt;
    procedure Set_height(AValue : LongInt);
    property height : LongInt read Get_height write Set_height;
    function Get_hspace : LongInt;
    procedure Set_hspace(AValue : LongInt);
    property hspace : LongInt read Get_hspace write Set_hspace;
    function Get_isMap : Boolean;
    procedure Set_isMap(AValue : Boolean);
    property isMap : Boolean read Get_isMap write Set_isMap;
    function Get_longDesc : DOMString;
    procedure Set_longDesc(AValue : DOMString);
    property longDesc : DOMString read Get_longDesc write Set_longDesc;
    function Get_src : DOMString;
    procedure Set_src(AValue : DOMString);
    property src : DOMString read Get_src write Set_src;
    function Get_useMap : DOMString;
    procedure Set_useMap(AValue : DOMString);
    property useMap : DOMString read Get_useMap write Set_useMap;
    function Get_vspace : LongInt;
    procedure Set_vspace(AValue : LongInt);
    property vspace : LongInt read Get_vspace write Set_vspace;
    function Get_width : LongInt;
    procedure Set_width(AValue : LongInt);
    property width : LongInt read Get_width write Set_width;
  end;

  IHTMLObjectElement = interface(IHTMLElement)
  ['{6227FF10-32EE-4BC2-863C-8991374F29F9}']
    function Get_form : IHTMLFormElement;
    property form : IHTMLFormElement read Get_form;
    function Get_contentDocument : IHTMLDocument;
    property contentDocument : IHTMLDocument read Get_contentDocument;
    function Get_code : DOMString;
    procedure Set_code(AValue : DOMString);
    property code : DOMString read Get_code write Set_code;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_archive : DOMString;
    procedure Set_archive(AValue : DOMString);
    property archive : DOMString read Get_archive write Set_archive;
    function Get_border : DOMString;
    procedure Set_border(AValue : DOMString);
    property border : DOMString read Get_border write Set_border;
    function Get_codeBase : DOMString;
    procedure Set_codeBase(AValue : DOMString);
    property codeBase : DOMString read Get_codeBase write Set_codeBase;
    function Get_codeType : DOMString;
    procedure Set_codeType(AValue : DOMString);
    property codeType : DOMString read Get_codeType write Set_codeType;
    function Get_data : DOMString;
    procedure Set_data(AValue : DOMString);
    property data : DOMString read Get_data write Set_data;
    function Get_declare : Boolean;
    procedure Set_declare(AValue : Boolean);
    property declare : Boolean read Get_declare write Set_declare;
    function Get_height : DOMString;
    procedure Set_height(AValue : DOMString);
    property height : DOMString read Get_height write Set_height;
    function Get_hspace : LongInt;
    procedure Set_hspace(AValue : LongInt);
    property hspace : LongInt read Get_hspace write Set_hspace;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_standby : DOMString;
    procedure Set_standby(AValue : DOMString);
    property standby : DOMString read Get_standby write Set_standby;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
    function Get_useMap : DOMString;
    procedure Set_useMap(AValue : DOMString);
    property useMap : DOMString read Get_useMap write Set_useMap;
    function Get_vspace : LongInt;
    procedure Set_vspace(AValue : LongInt);
    property vspace : LongInt read Get_vspace write Set_vspace;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
  end;

  IHTMLParamElement = interface(IHTMLElement)
  ['{BBE7A59A-3F84-48A0-8133-B14E17DE1828}']
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
    function Get_value : DOMString;
    procedure Set_value(AValue : DOMString);
    property value : DOMString read Get_value write Set_value;
    function Get_valueType : DOMString;
    procedure Set_valueType(AValue : DOMString);
    property valueType : DOMString read Get_valueType write Set_valueType;
  end;

  IHTMLAppletElement = interface(IHTMLElement)
  ['{2BCF0631-D912-4921-9737-7384195C617F}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_alt : DOMString;
    procedure Set_alt(AValue : DOMString);
    property alt : DOMString read Get_alt write Set_alt;
    function Get_archive : DOMString;
    procedure Set_archive(AValue : DOMString);
    property archive : DOMString read Get_archive write Set_archive;
    function Get_code : DOMString;
    procedure Set_code(AValue : DOMString);
    property code : DOMString read Get_code write Set_code;
    function Get_codeBase : DOMString;
    procedure Set_codeBase(AValue : DOMString);
    property codeBase : DOMString read Get_codeBase write Set_codeBase;
    function Get_height : DOMString;
    procedure Set_height(AValue : DOMString);
    property height : DOMString read Get_height write Set_height;
    function Get_hspace : LongInt;
    procedure Set_hspace(AValue : LongInt);
    property hspace : LongInt read Get_hspace write Set_hspace;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_object : DOMString;
    procedure Set_object(AValue : DOMString);
    property _object : DOMString read Get_object write Set_object;
    function Get_vspace : LongInt;
    procedure Set_vspace(AValue : LongInt);
    property vspace : LongInt read Get_vspace write Set_vspace;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
  end;

  IHTMLMapElement = interface(IHTMLElement)
  ['{ABE90F56-14C6-4015-95CA-4A303FF3CA91}']
    function Get_areas : IHTMLCollection;
    property areas : IHTMLCollection read Get_areas;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
  end;

  IHTMLAreaElement = interface(IHTMLElement)
  ['{64AD073E-8C94-4AB9-9968-E61D5D30FCD6}']
    function Get_accessKey : DOMString;
    procedure Set_accessKey(AValue : DOMString);
    property accessKey : DOMString read Get_accessKey write Set_accessKey;
    function Get_alt : DOMString;
    procedure Set_alt(AValue : DOMString);
    property alt : DOMString read Get_alt write Set_alt;
    function Get_coords : DOMString;
    procedure Set_coords(AValue : DOMString);
    property coords : DOMString read Get_coords write Set_coords;
    function Get_href : DOMString;
    procedure Set_href(AValue : DOMString);
    property href : DOMString read Get_href write Set_href;
    function Get_noHref : Boolean;
    procedure Set_noHref(AValue : Boolean);
    property noHref : Boolean read Get_noHref write Set_noHref;
    function Get_shape : DOMString;
    procedure Set_shape(AValue : DOMString);
    property shape : DOMString read Get_shape write Set_shape;
    function Get_tabIndex : LongInt;
    procedure Set_tabIndex(AValue : LongInt);
    property tabIndex : LongInt read Get_tabIndex write Set_tabIndex;
    function Get_target : DOMString;
    procedure Set_target(AValue : DOMString);
    property target : DOMString read Get_target write Set_target;
  end;

  IHTMLScriptElement = interface(IHTMLElement)
  ['{0919BE5C-363D-4F63-AE25-33B2C8767C7D}']
    function Get_text : DOMString;
    procedure Set_text(AValue : DOMString);
    property text : DOMString read Get_text write Set_text;
    function Get_htmlFor : DOMString;
    procedure Set_htmlFor(AValue : DOMString);
    property htmlFor : DOMString read Get_htmlFor write Set_htmlFor;
    function Get_event : DOMString;
    procedure Set_event(AValue : DOMString);
    property event : DOMString read Get_event write Set_event;
    function Get_charset : DOMString;
    procedure Set_charset(AValue : DOMString);
    property charset : DOMString read Get_charset write Set_charset;
    function Get_defer : Boolean;
    procedure Set_defer(AValue : Boolean);
    property defer : Boolean read Get_defer write Set_defer;
    function Get_src : DOMString;
    procedure Set_src(AValue : DOMString);
    property src : DOMString read Get_src write Set_src;
    function Get_type : DOMString;
    procedure Set_type(AValue : DOMString);
    property _type : DOMString read Get_type write Set_type;
  end;

  IHTMLTableCaptionElement = interface;
  IHTMLTableSectionElement = interface;

  IHTMLTableElement = interface(IHTMLElement)
  ['{91181B28-0587-4F28-B605-85EA7033DBA9}']
    function Get_rows : IHTMLCollection;
    property rows : IHTMLCollection read Get_rows;
    function Get_tBodies : IHTMLCollection;
    property tBodies : IHTMLCollection read Get_tBodies;
    function Get_caption : IHTMLTableCaptionElement;
    procedure Set_caption(AValue : IHTMLTableCaptionElement);
    property caption : IHTMLTableCaptionElement read Get_caption write Set_caption;
    function Get_tHead : IHTMLTableSectionElement;
    procedure Set_tHead(AValue : IHTMLTableSectionElement);
    property tHead : IHTMLTableSectionElement read Get_tHead write Set_tHead;
    function Get_tFoot : IHTMLTableSectionElement;
    procedure Set_tFoot(AValue : IHTMLTableSectionElement);
    property tFoot : IHTMLTableSectionElement read Get_tFoot write Set_tFoot;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_bgColor : DOMString;
    procedure Set_bgColor(AValue : DOMString);
    property bgColor : DOMString read Get_bgColor write Set_bgColor;
    function Get_border : DOMString;
    procedure Set_border(AValue : DOMString);
    property border : DOMString read Get_border write Set_border;
    function Get_cellPadding : DOMString;
    procedure Set_cellPadding(AValue : DOMString);
    property cellPadding : DOMString read Get_cellPadding write Set_cellPadding;
    function Get_cellSpacing : DOMString;
    procedure Set_cellSpacing(AValue : DOMString);
    property cellSpacing : DOMString read Get_cellSpacing write Set_cellSpacing;
    function Get_frame : DOMString;
    procedure Set_frame(AValue : DOMString);
    property frame : DOMString read Get_frame write Set_frame;
    function Get_rules : DOMString;
    procedure Set_rules(AValue : DOMString);
    property rules : DOMString read Get_rules write Set_rules;
    function Get_summary : DOMString;
    procedure Set_summary(AValue : DOMString);
    property summary : DOMString read Get_summary write Set_summary;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
    function createTHead() : IHTMLElement;
    procedure deleteTHead();
    function createTFoot() : IHTMLElement;
    procedure deleteTFoot();
    function createCaption() : IHTMLElement;
    procedure deleteCaption();
    function insertRow(index : LongInt) : IHTMLElement;
    procedure deleteRow(index : LongInt);
  end;

  IHTMLTableCaptionElement = interface(IHTMLElement)
  ['{FDE0BCF9-1CD6-494D-9DF9-B0B0FE06FDE5}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
  end;

  IHTMLTableColElement = interface(IHTMLElement)
  ['{78F6A7DF-60A4-4EEF-AFF4-56BB40074A3B}']
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_ch : DOMString;
    procedure Set_ch(AValue : DOMString);
    property ch : DOMString read Get_ch write Set_ch;
    function Get_chOff : DOMString;
    procedure Set_chOff(AValue : DOMString);
    property chOff : DOMString read Get_chOff write Set_chOff;
    function Get_span : LongInt;
    procedure Set_span(AValue : LongInt);
    property span : LongInt read Get_span write Set_span;
    function Get_vAlign : DOMString;
    procedure Set_vAlign(AValue : DOMString);
    property vAlign : DOMString read Get_vAlign write Set_vAlign;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
  end;

  IHTMLTableSectionElement = interface(IHTMLElement)
  ['{D5E997E1-CA2B-4D33-BF32-892F4F82EA21}']
    function Get_rows : IHTMLCollection;
    property rows : IHTMLCollection read Get_rows;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_ch : DOMString;
    procedure Set_ch(AValue : DOMString);
    property ch : DOMString read Get_ch write Set_ch;
    function Get_chOff : DOMString;
    procedure Set_chOff(AValue : DOMString);
    property chOff : DOMString read Get_chOff write Set_chOff;
    function Get_vAlign : DOMString;
    procedure Set_vAlign(AValue : DOMString);
    property vAlign : DOMString read Get_vAlign write Set_vAlign;
    function insertRow(index : LongInt) : IHTMLElement;
    procedure deleteRow(index : LongInt);
  end;

  IHTMLTableRowElement = interface(IHTMLElement)
  ['{C479A948-BBD2-4EB7-9282-64DEA24C7EF2}']
    function Get_rowIndex : LongInt;
    property rowIndex : LongInt read Get_rowIndex;
    function Get_sectionRowIndex : LongInt;
    property sectionRowIndex : LongInt read Get_sectionRowIndex;
    function Get_cells : IHTMLCollection;
    property cells : IHTMLCollection read Get_cells;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_bgColor : DOMString;
    procedure Set_bgColor(AValue : DOMString);
    property bgColor : DOMString read Get_bgColor write Set_bgColor;
    function Get_ch : DOMString;
    procedure Set_ch(AValue : DOMString);
    property ch : DOMString read Get_ch write Set_ch;
    function Get_chOff : DOMString;
    procedure Set_chOff(AValue : DOMString);
    property chOff : DOMString read Get_chOff write Set_chOff;
    function Get_vAlign : DOMString;
    procedure Set_vAlign(AValue : DOMString);
    property vAlign : DOMString read Get_vAlign write Set_vAlign;
    function insertCell(index : LongInt) : IHTMLElement;
    procedure deleteCell(index : LongInt);
  end;

  IHTMLTableCellElement = interface(IHTMLElement)
  ['{BA5D860E-7CCF-42FD-8EE3-3F2E14538826}']
    function Get_cellIndex : LongInt;
    property cellIndex : LongInt read Get_cellIndex;
    function Get_abbr : DOMString;
    procedure Set_abbr(AValue : DOMString);
    property abbr : DOMString read Get_abbr write Set_abbr;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_axis : DOMString;
    procedure Set_axis(AValue : DOMString);
    property axis : DOMString read Get_axis write Set_axis;
    function Get_bgColor : DOMString;
    procedure Set_bgColor(AValue : DOMString);
    property bgColor : DOMString read Get_bgColor write Set_bgColor;
    function Get_ch : DOMString;
    procedure Set_ch(AValue : DOMString);
    property ch : DOMString read Get_ch write Set_ch;
    function Get_chOff : DOMString;
    procedure Set_chOff(AValue : DOMString);
    property chOff : DOMString read Get_chOff write Set_chOff;
    function Get_colSpan : LongInt;
    procedure Set_colSpan(AValue : LongInt);
    property colSpan : LongInt read Get_colSpan write Set_colSpan;
    function Get_headers : DOMString;
    procedure Set_headers(AValue : DOMString);
    property headers : DOMString read Get_headers write Set_headers;
    function Get_height : DOMString;
    procedure Set_height(AValue : DOMString);
    property height : DOMString read Get_height write Set_height;
    function Get_noWrap : Boolean;
    procedure Set_noWrap(AValue : Boolean);
    property noWrap : Boolean read Get_noWrap write Set_noWrap;
    function Get_rowSpan : LongInt;
    procedure Set_rowSpan(AValue : LongInt);
    property rowSpan : LongInt read Get_rowSpan write Set_rowSpan;
    function Get_scope : DOMString;
    procedure Set_scope(AValue : DOMString);
    property scope : DOMString read Get_scope write Set_scope;
    function Get_vAlign : DOMString;
    procedure Set_vAlign(AValue : DOMString);
    property vAlign : DOMString read Get_vAlign write Set_vAlign;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
  end;

  IHTMLFrameSetElement = interface(IHTMLElement)
  ['{D48E62AC-DE65-4145-BDC8-C3B073B35418}']
    function Get_cols : DOMString;
    procedure Set_cols(AValue : DOMString);
    property cols : DOMString read Get_cols write Set_cols;
    function Get_rows : DOMString;
    procedure Set_rows(AValue : DOMString);
    property rows : DOMString read Get_rows write Set_rows;
  end;

  IHTMLFrameElement = interface(IHTMLElement)
  ['{8E56F52C-F122-4CB2-AD07-E82A54B7D019}']
    function Get_contentDocument : IHTMLDocument;
    property contentDocument : IHTMLDocument read Get_contentDocument;
    function Get_frameBorder : DOMString;
    procedure Set_frameBorder(AValue : DOMString);
    property frameBorder : DOMString read Get_frameBorder write Set_frameBorder;
    function Get_longDesc : DOMString;
    procedure Set_longDesc(AValue : DOMString);
    property longDesc : DOMString read Get_longDesc write Set_longDesc;
    function Get_marginHeight : DOMString;
    procedure Set_marginHeight(AValue : DOMString);
    property marginHeight : DOMString read Get_marginHeight write Set_marginHeight;
    function Get_marginWidth : DOMString;
    procedure Set_marginWidth(AValue : DOMString);
    property marginWidth : DOMString read Get_marginWidth write Set_marginWidth;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_noResize : Boolean;
    procedure Set_noResize(AValue : Boolean);
    property noResize : Boolean read Get_noResize write Set_noResize;
    function Get_scrolling : DOMString;
    procedure Set_scrolling(AValue : DOMString);
    property scrolling : DOMString read Get_scrolling write Set_scrolling;
    function Get_src : DOMString;
    procedure Set_src(AValue : DOMString);
    property src : DOMString read Get_src write Set_src;
   end;

  IHTMLIFrameElement = interface(IHTMLElement)
  ['{D68B772C-83B3-4F5D-A06C-EB8918558B1E}']
    function Get_contentDocument : IHTMLDocument;
    property contentDocument : IHTMLDocument read Get_contentDocument;
    function Get_align : DOMString;
    procedure Set_align(AValue : DOMString);
    property align : DOMString read Get_align write Set_align;
    function Get_frameBorder : DOMString;
    procedure Set_frameBorder(AValue : DOMString);
    property frameBorder : DOMString read Get_frameBorder write Set_frameBorder;
    function Get_height : DOMString;
    procedure Set_height(AValue : DOMString);
    property height : DOMString read Get_height write Set_height;
    function Get_longDesc : DOMString;
    procedure Set_longDesc(AValue : DOMString);
    property longDesc : DOMString read Get_longDesc write Set_longDesc;
    function Get_marginHeight : DOMString;
    procedure Set_marginHeight(AValue : DOMString);
    property marginHeight : DOMString read Get_marginHeight write Set_marginHeight;
    function Get_marginWidth : DOMString;
    procedure Set_marginWidth(AValue : DOMString);
    property marginWidth : DOMString read Get_marginWidth write Set_marginWidth;
    function Get_name : DOMString;
    procedure Set_name(AValue : DOMString);
    property name : DOMString read Get_name write Set_name;
    function Get_scrolling : DOMString;
    procedure Set_scrolling(AValue : DOMString);
    property scrolling : DOMString read Get_scrolling write Set_scrolling;
    function Get_src : DOMString;
    procedure Set_src(AValue : DOMString);
    property src : DOMString read Get_src write Set_src;
    function Get_width : DOMString;
    procedure Set_width(AValue : DOMString);
    property width : DOMString read Get_width write Set_width;
  end;

implementation

end.
