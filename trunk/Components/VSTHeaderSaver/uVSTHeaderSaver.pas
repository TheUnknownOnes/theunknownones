//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uVSTHeaderSaver;

interface

{$R 'images.res'}

uses
  SysUtils, Classes, Controls, VirtualTrees, msxml, uXMLTools;

type
  TNeedXMLNodeProc = procedure(var AXMLNode : IXMLDOMNode) of object;

  TSaveField = (sfPosition=0,
                sfWidth=1,
                sfAllowClick=2,
                sfDraggable=3,
                sfEnabled=4,
                sfParentColor=5,
                sfResizable=6,
                sfShowDropMark=7,
                sfVisible=8,
                sfAutoSpring=9,
                sfFixed=10);

  TVSTHeaderSaver = class(TComponent)
  private
    FSaveFields : array[Low(TSaveField)..High(TSaveField)] of Boolean;

    FTree: TVirtualStringTree;
    FNeedNode: TNeedXMLNodeProc;

    procedure SaveHeaderSettings(var Node : IXMLDOMNode; AHeader : TVirtualTreeColumn);
    procedure LoadHeaderSettings(var Node : IXMLDOMNode; AHeader : TVirtualTreeColumn);

    function NeedNode(var ANode : IXMLDOMNode) : Boolean;
    function GetSaveField(const Index: Integer): Boolean;
    procedure SetSaveField(const Index: Integer; const Value: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    procedure LoadSettings();
    procedure SaveSettings();
  published
    property VST : TVirtualStringTree read FTree write FTree;
    property OnNeedXMLNode : TNeedXMLNodeProc read FNeedNode write FNeedNode;

    property SavePosition     : Boolean index sfPosition      read GetSaveField write SetSaveField default true;
    property SaveWidth        : Boolean index sfWidth         read GetSaveField write SetSaveField default true;
    property SaveAllowClick   : Boolean index sfAllowClick    read GetSaveField write SetSaveField default false;
    property SaveDraggable    : Boolean index sfDraggable     read GetSaveField write SetSaveField default false;
    property SaveEnabled      : Boolean index sfEnabled       read GetSaveField write SetSaveField default false;
    property SaveParentColor  : Boolean index sfParentColor   read GetSaveField write SetSaveField default false;
    property SaveResizeable   : Boolean index sfResizable     read GetSaveField write SetSaveField default false;
    property SaveShowDropMark : Boolean index sfShowDropMark  read GetSaveField write SetSaveField default false;
    property SaveVisible      : Boolean index sfVisible       read GetSaveField write SetSaveField default true;
    property SaveAutoSpring   : Boolean index sfAutoSpring    read GetSaveField write SetSaveField default false;
    property SaveFixed        : Boolean index sfFixed         read GetSaveField write SetSaveField default true;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TVSTHeaderSaver]);
end;

{ TVSTHeaderSaver }

constructor TVSTHeaderSaver.Create(AOwner: TComponent);
begin
  inherited;

  FSaveFields[sfPosition]:=true;
  FSaveFields[sfWidth]:=true;
  FSaveFields[sfAllowClick]:=false;
  FSaveFields[sfDraggable]:=false;
  FSaveFields[sfEnabled]:=false;
  FSaveFields[sfParentColor]:=false;
  FSaveFields[sfResizable]:=false;
  FSaveFields[sfShowDropMark]:=false;
  FSaveFields[sfVisible]:=true;
  FSaveFields[sfAutoSpring]:=false;
  FSaveFields[sfFixed]:=true;
end;

destructor TVSTHeaderSaver.Destroy;
begin
  inherited;
end;

function TVSTHeaderSaver.GetSaveField(const Index: Integer): Boolean;
begin
  Result:=FSaveFields[TSaveField(Index)];
end;

procedure TVSTHeaderSaver.LoadHeaderSettings(var Node: IXMLDOMNode;
  AHeader: TVirtualTreeColumn);
var
  idx : TSaveField;
  Attr : IXMLDOMNode;
begin
  for idx:=Low(TSaveField) to High(TSaveField) do
  begin
    if not FSaveFields[idx] then continue;
    
    Attr:=Node.attributes.getNamedItem('S'+IntToStr(Integer(idx)));
    if Assigned(Attr) then
    begin
      case idx of
        sfPosition:     AHeader.Position:=Attr.nodeValue;
        sfWidth:        AHeader.Width:=Attr.nodeValue;
        sfAllowClick:   if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coAllowClick]
                        else
                          AHeader.Options:=AHeader.Options-[coAllowClick];
        sfDraggable:    if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coDraggable]
                        else
                          AHeader.Options:=AHeader.Options-[coDraggable];
        sfEnabled:      if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coEnabled]
                        else
                          AHeader.Options:=AHeader.Options-[coEnabled];
        sfParentColor:  if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coParentColor]
                        else
                          AHeader.Options:=AHeader.Options-[coParentColor];
        sfResizable:    if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coResizable]
                        else
                          AHeader.Options:=AHeader.Options-[coResizable];
        sfShowDropMark: if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coShowDropMark]
                        else
                          AHeader.Options:=AHeader.Options-[coShowDropMark];
        sfVisible:      if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coVisible]
                        else
                          AHeader.Options:=AHeader.Options-[coVisible];
        sfAutoSpring:   if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coAutoSpring]
                        else
                          AHeader.Options:=AHeader.Options-[coAutoSpring];
        sfFixed:        if (Attr.nodeValue) then
                          AHeader.Options:=AHeader.Options+[coFixed]
                        else
                          AHeader.Options:=AHeader.Options-[coFixed];
      end;
    end;
  end;
end;

procedure TVSTHeaderSaver.LoadSettings;
var
  Node,
  RootNode : IXMLDOMNode;
  Header : TVirtualTreeColumn;
  idx : Integer;
begin
  if not Assigned(FTree) then exit;

  if NeedNode(RootNode) then
  begin
    for idx:=0 to FTree.Header.Columns.Count-1 do
    begin
      Header:=Ftree.Header.Columns[idx];

      Node:=RootNode.selectSingleNode('Header[@ID='''+IntToStr(Header.ID)+''']');
      if Assigned(Node) then
        LoadHeaderSettings(Node, Header);    
    end;
  end;
end;

function TVSTHeaderSaver.NeedNode(var ANode: IXMLDOMNode): Boolean;
begin
  Result:=Assigned(FNeedNode);
  if not Result then exit;

  FNeedNode(ANode);
  Result:=Assigned(ANode);
end;

procedure TVSTHeaderSaver.SaveHeaderSettings(var Node: IXMLDOMNode;
  AHeader: TVirtualTreeColumn);
var
  idx : TSaveField;
  AttrName : String;
begin
  for idx:=Low(TSaveField) to High(TSaveField) do
  begin
    if FSaveFields[idx] then
    begin
      AttrName:='S'+IntToStr(Integer(idx));
      
      case idx of
        sfPosition:     XAddAttribute(Node, AttrName, AHeader.Position);
        sfWidth:        XAddAttribute(Node, AttrName, AHeader.Width);
        sfAllowClick:   XAddAttribute(Node, AttrName, coAllowClick in AHeader.Options);
        sfDraggable:    XAddAttribute(Node, AttrName, coDraggable in AHeader.Options);
        sfEnabled:      XAddAttribute(Node, AttrName, coEnabled in AHeader.Options);
        sfParentColor:  XAddAttribute(Node, AttrName, coParentColor in AHeader.Options);
        sfResizable:    XAddAttribute(Node, AttrName, coResizable in AHeader.Options);
        sfShowDropMark: XAddAttribute(Node, AttrName, coShowDropMark in AHeader.Options);
        sfVisible:      XAddAttribute(Node, AttrName, coVisible in AHeader.Options);
        sfAutoSpring:   XAddAttribute(Node, AttrName, coAutoSpring in AHeader.Options);
        sfFixed:        XAddAttribute(Node, AttrName, coFixed in AHeader.Options);
      end;
    end;
  end;  
end;

procedure TVSTHeaderSaver.SaveSettings;
var
  Node,
  RootNode : IXMLDOMNode;
  idx : Integer;
  Header : TVirtualTreeColumn;
begin
  if not Assigned(FTree) then exit;

  if NeedNode(RootNode) then
  begin
    while RootNode.hasChildNodes do
      RootNode.removeChild(RootNode.firstChild);

    for idx:=0 to FTree.Header.Columns.Count-1 do
    begin
      Header:=FTree.Header.Columns[idx];

      Node:=RootNode.ownerDocument.createElement('Header');
      Node:=RootNode.appendChild(Node);
      XAddAttribute(Node,'ID',Header.ID);

      SaveHeaderSettings(Node, Header);
    end;
  end;
end;

procedure TVSTHeaderSaver.SetSaveField(const Index: Integer;
  const Value: Boolean);
begin
  FSaveFields[TSaveField(Index)]:=Value;
end;


end.
