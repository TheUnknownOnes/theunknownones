unit uFrameConfig3rdParty;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  uFrameConfigColor;

type
  TFrameConfig3rdParty = class(TFrame)
    FrameConfigColor1: TFrameConfigColor;
    GroupBox1:         TGroupBox;
  private
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    function GetProviderName: string;
    procedure SetProviderName(const Value: string);
  public
    property ProviderName: string read GetProviderName write SetProviderName;
    property SelectedColor: TColor read GetColor write SetColor;

    procedure Save;
  end;

implementation

uses
  uCustomHelpMain;

{$R *.dfm}

{ TFrame1 }

function TFrameConfig3rdParty.GetColor: TColor;
begin
  Result := FrameConfigColor1.SelectedColor;
end;

function TFrameConfig3rdParty.GetProviderName: string;
begin
  Result := GroupBox1.Caption;
end;

procedure TFrameConfig3rdParty.Save;
begin
  GlobalCustomHelp.Color[ProviderName] := SelectedColor;
end;

procedure TFrameConfig3rdParty.SetColor(const Value: TColor);
begin
  FrameConfigColor1.SelectedColor := Value;
end;

procedure TFrameConfig3rdParty.SetProviderName(const Value: string);
begin
  GroupBox1.Caption := Value;
  FrameConfigColor1.catbtnTopics.Categories[0].Caption := Value;
end;

end.
