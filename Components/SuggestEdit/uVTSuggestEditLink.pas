unit uVTSuggestEditLink;

interface

uses
  VTEditors, Windows, Controls, uSuggestEdit, Classes;

type
  TSuggestEditLink = class(TCustomEditLink)
  private
    FControl : TWinControl;
    function GetControl: TSuggestEdit;
    function GetDisplaySimilarity: Boolean;
    function GetIgnoreCase: Boolean;
    function GetSuggestBoxHeight: Integer;
    function GetThreshold: Integer;
    function GetWordList: TStrings;
    procedure SetDisplaySimilarity(const Value: Boolean);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetSuggestBoxHeight(const Value: Integer);
    procedure SetThreshold(const Value: Integer);
    procedure SetWordList(const Value: TStrings);
    function GetSuggestMethod: TSuggestMethod;
    procedure SetSuggestMethod(const Value: TSuggestMethod);
  protected
    procedure PrepareEditControl; override;
    function GetEditText: UnicodeString; override;
    procedure SetEditText(const Value: UnicodeString); override;
  public
    function CreateEditControl: TWinControl; override;
    property Tree;
    property SuggestEditControl : TSuggestEdit read GetControl;

    property Threshold: Integer read GetThreshold write SetThreshold;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property WordList: TStrings read GetWordList write SetWordList;
    property DisplaySimilarity: Boolean read GetDisplaySimilarity write SetDisplaySimilarity;
    property SuggestBoxHeight: Integer read GetSuggestBoxHeight write SetSuggestBoxHeight;
    property SuggestMethod: TSuggestMethod read GetSuggestMethod write SetSuggestMethod;
  end;

implementation

uses
  SysUtils;

{ TValidateEditEditLink }

function TSuggestEditLink.CreateEditControl: TWinControl;
begin
  if not Assigned(FControl) then
  begin
    FControl:=TSuggestEdit.Create(nil)
  end;

  Result:=FControl;
end;

function TSuggestEditLink.GetControl: TSuggestEdit;
begin
  Result:=TSuggestEdit(CreateEditControl);
end;

function TSuggestEditLink.GetDisplaySimilarity: Boolean;
begin
  Result:=SuggestEditControl.DisplaySimilarity;
end;

function TSuggestEditLink.GetEditText: UnicodeString;
begin
  Result:=SuggestEditControl.Text;
end;

function TSuggestEditLink.GetIgnoreCase: Boolean;
begin
  Result:=SuggestEditControl.IgnoreCase;
end;

function TSuggestEditLink.GetSuggestBoxHeight: Integer;
begin
  Result:=SuggestEditControl.SuggestBoxHeight;
end;

function TSuggestEditLink.GetSuggestMethod: TSuggestMethod;
begin
  Result:=SuggestEditControl.SuggestMethod;
end;

function TSuggestEditLink.GetThreshold: Integer;
begin
  Result:=SuggestEditControl.Threshold;
end;

function TSuggestEditLink.GetWordList: TStrings;
begin
  Result:=SuggestEditControl.WordList;
end;

procedure TSuggestEditLink.PrepareEditControl;
begin
  inherited;
end;

procedure TSuggestEditLink.SetDisplaySimilarity(const Value: Boolean);
begin
  SuggestEditControl.DisplaySimilarity :=Value;
end;

procedure TSuggestEditLink.SetEditText(const Value: UnicodeString);
begin
  inherited;
  SuggestEditControl.Text:=Value;
end;

procedure TSuggestEditLink.SetIgnoreCase(const Value: Boolean);
begin
  SuggestEditControl.IgnoreCase :=Value;
end;

procedure TSuggestEditLink.SetSuggestBoxHeight(const Value: Integer);
begin
  SuggestEditControl.SuggestBoxHeight :=Value;
end;

procedure TSuggestEditLink.SetSuggestMethod(const Value: TSuggestMethod);
begin
  SuggestEditControl.SuggestMethod:=Value;
end;

procedure TSuggestEditLink.SetThreshold(const Value: Integer);
begin
  SuggestEditControl.Threshold :=Value;
end;

procedure TSuggestEditLink.SetWordList(const Value: TStrings);
begin
  SuggestEditControl.WordList :=Value;
end;

end.
