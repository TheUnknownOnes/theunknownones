//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Grids, ValEdit, StrUtils, Registry,
  ToolsAPI;

type
  TformConfig = class(TForm)
    gbOptions: TGroupBox;
    panFixPrefix: TPanel;
    panContFixPrefixes: TPanel;
    panFixPrefixes: TPanel;
    valFixPrefixes: TValueListEditor;
    panAddFixPrefix: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnAddFixPrefix: TSpeedButton;
    cbComponentClass: TComboBox;
    edFixPrefix: TEdit;
    panContUseFixPrefix: TPanel;
    cbUseFixPrefixes: TCheckBox;
    panDelimter: TPanel;
    panContDelimiter: TPanel;
    panDelimiterEdit: TPanel;
    Label3: TLabel;
    edDelimiter: TEdit;
    panCBUseDelimiter: TPanel;
    cbUseDelimiter: TCheckBox;
    panBottom: TPanel;
    btnSaveExit: TBitBtn;
    btnCancel: TBitBtn;
    panUseIt: TPanel;
    cbUseCNA: TCheckBox;
    panCredits: TPanel;
    cbOnlyWithPrefix: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure valFixPrefixesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edFixPrefixChange(Sender: TObject);
    procedure cbUseDelimiterClick(Sender: TObject);
    procedure edFixPrefixKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbComponentClassKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnAddFixPrefixClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbUseFixPrefixesClick(Sender: TObject);
  private
  public
    procedure AddFixPrefix(CClass : String; Prefix : String; ClassIndex : Integer);
    function GetComponentName(ComponentClass : String;
                              var ComponentName : String;
                              Retry: Boolean;
                              FormEditor: IOTAFormEditor) : Integer;
    procedure SaveSettings();
    procedure LoadSettings();
  end;

const
  HINT_ADD_BUTTON='Add "$P" as prefix for "$C"';

  REG_COMPANY='TheUnknownOnes';
  REG_PROJECT='ComponentNamingAssistant';

implementation

uses unitCNAGetName;

{$R *.dfm}

{$region 'Form-Behaviour'}

procedure TformConfig.FormCreate(Sender: TObject);
begin
  //select the first component class
  if (cbComponentClass.Items.Count>0) then
    cbComponentClass.ItemIndex:=0;
end;

procedure TformConfig.FormShow(Sender: TObject);
begin
  //setup the form-layout
  cbUseFixPrefixesClick(Sender);
  cbUseDelimiterClick(Sender);
end;

procedure TformConfig.btnSaveExitClick(Sender: TObject);
begin
  SaveSettings;
  Close;
end;

procedure TformConfig.btnCancelClick(Sender: TObject);
begin
  LoadSettings;
  Close;
end;

{$endregion}

{$region 'FixPrefixes'}

procedure TformConfig.cbUseFixPrefixesClick(Sender: TObject);
begin
  panContFixPrefixes.Visible:=cbUseFixPrefixes.Checked;
end;

procedure TformConfig.btnAddFixPrefixClick(Sender: TObject);
var
  Prefix,
  CCLass      : String;
  i,CIndex    : Integer;
  FoundPrefix : Boolean;
begin
  Prefix:=Trim(edFixPrefix.Text);
  CCLass:=Trim(cbComponentClass.Text);

  //Empty prefix?
  if (AnsiSameText(Prefix,EmptyStr) or AnsiSameText(CCLass,EmptyStr)) then
  begin
    MessageDlg('Empty prefix/class-name specified!', mtWarning, [mbOK], 0);
    exit;
  end;

  //ComponentClass has already prefix ? Prefix already assigned?
  FoundPrefix:=false;
  for i:=0 to valFixPrefixes.RowCount-1 do
  begin
    if (AnsiSameText(Prefix,valFixPrefixes.Values[valFixPrefixes.Keys[i]])) then
    begin
      FoundPrefix:=true;
      exit;
    end;
  end;
  CIndex:=valFixPrefixes.Strings.IndexOfName(CClass);
  if (CIndex>-1) then
    if (MessageBox(0, PAnsiChar('This class has already a prefix!'+#13+#10+'Do you want to overwrite it?'), PAnsiChar('Class exists'), MB_ICONWARNING or MB_YESNO) = idNo) then
      exit;
  if (FoundPrefix) and not (CIndex>-1) then
    if (MessageBox(0, PAnsiChar('This prefix is already assigned to a class!'+#13+#10+'Do you want to assign ist multiple?'), PAnsiChar('Prefix exists'), MB_ICONWARNING or MB_YESNO) = idNo) then
      exit;

  //now add this pair
  AddFixPrefix(CCLass,Prefix,CIndex);

  //clear the input-controls
  cbComponentClass.Text:=EmptyStr;
  edFixPrefix.Text:=EmptyStr;

  //select the next item
  if (cbComponentClass.Items.Count>0) then
    cbComponentClass.ItemIndex:=0;
end;

procedure TformConfig.AddFixPrefix(CClass, Prefix: String; ClassIndex: Integer);
var
  i : Integer;
begin
  //update existing entry?
  if (ClassIndex>-1) then
    valFixPrefixes.Values[valFixPrefixes.Keys[ClassIndex]]:=Prefix
  else
    valFixPrefixes.Strings.Add(CClass+'='+Prefix);

  //delete the componentclass-entry out of the combo
  i:=cbComponentClass.Items.IndexOf(CClass);
  if (i>-1) then
    cbComponentClass.Items.Delete(i);
end;

procedure TformConfig.edFixPrefixChange(Sender: TObject);
begin
  btnAddFixPrefix.Hint:=StringReplace(HINT_ADD_BUTTON,'$P',edFixPrefix.Text,[]);
  btnAddFixPrefix.Hint:=StringReplace(btnAddFixPrefix.Hint,'$C',cbComponentClass.Text,[]);
end;

procedure TformConfig.edFixPrefixKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) then btnAddFixPrefixClick(Sender);
end;

procedure TformConfig.cbComponentClassKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) then btnAddFixPrefixClick(Sender);
end;

procedure TformConfig.valFixPrefixesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CClass : String;
begin
  //del pressed?
  if (Key=VK_DELETE) then
  begin
    //a row selected?
    if (valFixPrefixes.Row>-1) then
    begin
      //remember classname
      CClass:=valFixPrefixes.Keys[valFixPrefixes.Row];
      //delete Row
      valFixPrefixes.DeleteRow(valFixPrefixes.Row);
      //add classname to combo, maybe we will re-use it
      if (cbComponentClass.Items.IndexOf(CClass)=-1) then
        cbComponentClass.Items.Add(CClass);
    end;
  end;
end;

{$endregion}

{$region 'Delimiter'}

procedure TformConfig.cbUseDelimiterClick(Sender: TObject);
begin
  panContDelimiter.Visible:=cbUseDelimiter.Checked;
end;

{$endregion}

{$region 'Helper'}

procedure TformConfig.SaveSettings;
var
  Regi : TRegistry;
  i : Integer;
  Values : TStrings;
begin
  Values:=TStringList.Create;
  Regi:=TRegistry.Create;

  try
    try
      Regi.RootKey:=HKEY_CURRENT_USER;

      //save general data
      if (Regi.OpenKey('\Software\'+REG_COMPANY+'\'+REG_PROJECT+'\Settings\General\',true)) then
      begin
        Regi.WriteBool(EmptyStr,cbUseCNA.Checked);
        Regi.WriteBool('OnlyIfAssigned',cbOnlyWithPrefix.Checked);
        Regi.CloseKey;
      end;

      //lets care about the fix-prefixes
      if (Regi.OpenKey('\Software\'+REG_COMPANY+'\'+REG_PROJECT+'\Settings\FixPrefixes\',true)) then
      begin
        //delete all existing Prefixes
        Regi.GetValueNames(Values);
        for i:=0 to Values.Count-1 do
          Regi.DeleteValue(Values[i]);

        //add all known prefixes; start at 1 because 0 is the headline
        for i:=1 to valFixPrefixes.RowCount-1 do
          Regi.WriteString(valFixPrefixes.Keys[i],valFixPrefixes.Values[valFixPrefixes.Keys[i]]);

        //save, if the user wants to use the fix_prefixes
        Regi.WriteBool(EmptyStr,cbUseFixPrefixes.Checked);

        Regi.CloseKey;
      end;

      //now, save settings about the delimiter
      if (Regi.OpenKey('\Software\'+REG_COMPANY+'\'+REG_PROJECT+'\Settings\Delimiter\',true)) then
      begin
        Regi.WriteString('Delimiter',edDelimiter.Text);
        Regi.WriteBool(EmptyStr,cbUseDelimiter.Checked);
        Regi.CloseKey;
      end;
    except end;
  finally
    Regi.CloseKey;
    Regi.Free;
    Values.Free;
  end;
end;

procedure TformConfig.LoadSettings;
var
  Regi : TRegistry;
  Values : TStrings;
  Prefix : String;
  i : Integer;
begin
  Values:=TStringList.Create;
  Regi:=TRegistry.Create;

  try
    try
      if (Regi.OpenKey('\Software\'+REG_COMPANY+'\'+REG_PROJECT+'\Settings\General\',false)) then
      begin
        cbUseCNA.Checked:=Regi.ReadBool(EmptyStr);
        cbOnlyWithPrefix.Checked:=Regi.ReadBool('OnlyIfAssigned');
        Regi.CloseKey;
      end;

      //Lets load the FixPrefixes
      if (Regi.OpenKey('\Software\'+REG_COMPANY+'\'+REG_PROJECT+'\Settings\FixPrefixes\',false)) then
      begin
        valFixPrefixes.Strings.Clear;
        //load all ClassNames
        Regi.GetValueNames(Values);
        for i:=0 to Values.Count-1 do
        begin
          //skip the default-value
          if (AnsiSameText(Values[i],EmptyStr)) then continue;
          //read the prefix for this classname
          Prefix:=Regi.ReadString(Values[i]);
          //add pair to the list
          AddFixPrefix(Values[i],Prefix,valFixPrefixes.Strings.IndexOfName(Values[i]));
        end;

        //load the use-switch
        cbUseFixPrefixes.Checked:=Regi.ReadBool(EmptyStr);

        Regi.CloseKey;
      end;
      if (Regi.OpenKey('\Software\'+REG_COMPANY+'\'+REG_PROJECT+'\Settings\Delimiter\',false)) then
      begin
        if (Regi.ValueExists('Delimiter')) then
          edDelimiter.Text:=Regi.ReadString('Delimiter');
        if (Regi.ValueExists(EmptyStr)) then
          cbUseDelimiter.Checked:=Regi.ReadBool(EmptyStr);
        Regi.CloseKey;
      end;
    except end;
  finally
    Regi.CloseKey;
    Regi.Free;
    Values.Free;
  end;

  //select the next item
  if (cbComponentClass.Items.Count>0) then
    cbComponentClass.ItemIndex:=0;
end;

{$endregion}

function TformConfig.GetComponentName(ComponentClass : String;
                              var ComponentName : String;
                              Retry: Boolean;
                              FormEditor: IOTAFormEditor): Integer;
var
  Prefix : String;
  formGetName : TformGetName;

  function GetUniqueComponentName: String;
  var
    CompCount : Integer;
  begin
    CompCount:=0;

    repeat
      inc(CompCount);
      result:=prefix+AnsiRightStr(ComponentClass,Length(ComponentClass)-1)+IntToStr(CompCount);
	  until FormEditor.FindComponent(Result)=nil;
  end;

begin
  Prefix:=EmptyStr;

  //should we use a fix-prefix?
  if (cbUseFixPrefixes.Checked) then
    Prefix:=valFixPrefixes.Values[ComponentClass];

  //should we use a delimiter?
  if (cbUseDelimiter.Checked) then
    Prefix:=Prefix+edDelimiter.Text;

  formGetName:=TformGetName.Create(nil);

  if Retry then
  begin
    formGetName.edName.Text:=ComponentName;
    formGetName.PrefixLength:=-1;
  end
  else
  begin
    formGetName.edName.Text:=GetUniqueComponentName;
    formGetName.PrefixLength:=Length(Prefix);
  end;
  
  Result:=formGetName.ShowModal;
  ComponentName:=Trim(formGetName.edName.Text);

  formGetName.Release;
  formGetName:=nil;
end;

end.
