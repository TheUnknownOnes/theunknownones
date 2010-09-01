unit uch2Provider3rdPartyHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, Registry, HelpIntfs, StdCtrls;

type
  Tch2Provider3rdPartyHelp = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);
    procedure ShowHelp(AID : Integer);
    procedure Configure;

    function GetPriority : Integer;
    {$ENDREGION}

  end;

  Tch2FormProvider3rdPartyHelp = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    class procedure Execute;
  end;

implementation

{$R *.dfm}

function GetImplementingObject(const I: IInterface): TObject;
const
  AddByte = $04244483;
  AddLong = $04244481;
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte: shortint);
      AddLong : (AdjustmentLong: longint);
    end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := Pointer(I);
  if Assigned(Result) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Inc(PAnsiChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong: Inc(PAnsiChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    except
      Result := nil;
    end;
end;

type
  THelpManagerHack = class(TInterfacedObject)
  private
    FHelpSelector: IHelpSelector;
    FViewerList: TList;
  end;

  THelpViewerNodeHack = class(TObject)
  private
    FViewer: ICustomHelpViewer;
  end;

function GetHelpManagerHackObject: THelpManagerHack;
begin
  Result:=THelpManagerHack(GetImplementingObject(ch2Main.HelpManager));
end;

{ Tch2Provider3rdPartyHelp }

const
  Setttings_Value_Priority = 'Priority';

procedure Tch2Provider3rdPartyHelp.AfterConstruction;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(Setttings_Value_Priority) then
        FPriority := reg.ReadInteger(Setttings_Value_Priority)
      else
        FPriority := 0;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2Provider3rdPartyHelp.BeforeDestruction;
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(Setttings_Value_Priority, FPriority);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  inherited;
end;

procedure Tch2Provider3rdPartyHelp.Configure;
begin
  Tch2FormProvider3rdPartyHelp.Execute;
end;

function Tch2Provider3rdPartyHelp.GetDescription: String;
begin
  Result := 'Query other Help providers installed';
end;

function Tch2Provider3rdPartyHelp.GetGUID: TGUID;
const
  g : TGUID = '{A40591A4-1CAC-4F31-A222-EC71AAC0622C}';
begin
  Result := g;
end;

function Tch2Provider3rdPartyHelp.GetName: String;
begin
  Result:='3rd party help';
end;

function Tch2Provider3rdPartyHelp.GetPriority: Integer;
begin
  Result:=FPriority;
end;

procedure Tch2Provider3rdPartyHelp.ProvideHelp(AKeyword: String; AGUI: Ich2GUI);
begin

end;

procedure Tch2Provider3rdPartyHelp.ShowHelp(AID: Integer);
begin

end;

{ Tch2FormProvider3rdPartyHelp }

procedure Tch2FormProvider3rdPartyHelp.Button1Click(Sender: TObject);
var
  ViewerList : TList;
  P : Pointer;
  Node : THelpViewerNodeHack absolute P;
begin
  ViewerList:=GetHelpManagerHackObject.FViewerList;
  showmessage(IntToStr(ViewerList.Count));
  for P in ViewerList do
  begin

      ListBox1.AddItem(Node.FViewer.GetViewerName, Node);

  end;

end;

class procedure Tch2FormProvider3rdPartyHelp.Execute;
var
  Form : Tch2FormProvider3rdPartyHelp;
begin
  Form:=Tch2FormProvider3rdPartyHelp.Create(nil);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure Tch2FormProvider3rdPartyHelp.ListBox1DblClick(Sender: TObject);
var 
  NOde: THelpViewerNodeHack;
  sl : TStrings;
begin
  Node:=THelpViewerNodeHack(ListBox1.Items.Objects[ListBox1.ItemIndex]);
  if Assigned(Node) then
  begin
    if Node.FViewer.UnderstandsKeyword(Edit1.Text)>0 then
    begin    
      sl:=NOde.FViewer.GetHelpStrings(Edit1.Text);
      showMessage(sl.Text);
      sl.Free;
    end;
  end;
  
  
end;

initialization
  ch2Main.RegisterProvider(Tch2Provider3rdPartyHelp.Create as Ich2Provider);

end.
