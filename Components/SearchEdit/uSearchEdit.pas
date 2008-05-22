unit uSearchEdit;

interface

uses
  StdCtrls, uEnumStringList, Classes, ShlObj, ComObj, Controls;

type
  TCustomSearchEdit = class(TEdit)
  private
    FItems : TEnumStringList;
    FAutoComplete: IAutoComplete2;
    procedure SetItems(const Value: TStrings);
    function GetItems: TStrings;
  protected
    procedure SetParent(AParent: TWinControl); override;

    property Items: TStrings read GetItems write SetItems;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TSearchEdit = class(TCustomSearchEdit)
  published
    property Items;
  end;

implementation

{ TCustomSearchEdit }

constructor TCustomSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
  FItems:=TEnumStringList.Create; 
end;

destructor TCustomSearchEdit.Destroy;
begin
  FAutoComplete:=Nil;
  if Assigned(FItems) then
    FItems.Free;
  inherited;
end;

function TCustomSearchEdit.GetItems: TStrings;
begin
  Result:=TStrings(FItems);
end;

procedure TCustomSearchEdit.SetItems(const Value: TStrings);
begin
  FItems.Text:=Value.Text;
end;

procedure TCustomSearchEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if not (csDesigning in Self.ComponentState) and
     not (csDestroying in Self.ComponentState) and
     not Assigned(FAutoComplete) then
  begin
    FAutoComplete  := CreateComObject(CLSID_AutoComplete) as IAutoComplete2;
    OleCheck(FAutoComplete.SetOptions(ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST));
    OleCheck(FAutoComplete.Init(self.Handle, FItems.DefaultInterface, nil, nil));
  end;
end;

end.
