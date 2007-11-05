//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAMain;

{$I '..\CNA_Includes\main_include.pas'}
{$I '..\CNA_Includes\lang_ver.pas'}

interface
uses SysUtils, TypInfo, ToolsApi, Classes, Dialogs, Buttons, Forms, ExtCtrls,
     Controls, CategoryButtons, Menus, Variants, ComCtrls, Math, IniFiles,
     Registry, Windows, StrUtils, Graphics, UnitCNAData, ActnPopUp,
     CodeTemplateAPI;

procedure Register;

procedure FindProperties(ClassName : String; var Properties : TStrings);

const
  REG_ROOT='Software\TheUnknownOnes\';
  REG_PROGRAM=REG_ROOT+'ComponentNamingAssistant\';
  REG_CONFIG=REG_PROGRAM+'Settings\';

  PROP_FLAGS_SHOW_INPUT_DLG=1;

implementation

uses UnitCNATypes, unitCNAConfig, unitCNALangs, UnitCNAEditName,
    UnitCNAEditProperty, UnitCNAEditPropertyEnumeration,
    UnitCNAEditPropertySet, UnitCNAEditPropertyInteger, UnitCNAEditPropertyString,
    UnitCNAEditPropertyFloat;

type
  TWizardCNA = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    FButton          : TToolButton;
    FPopUp           : TPopupActionBar;
    FMIProfiles      : TMenuItem;

    FToolForm   : TForm;
    FCatButtons : TCategoryButtons;
    //FPanel      : TPanel;
    WizardDataModule : TCNADataModule;
    FSettings   : TCNASettings;

    function GetState: TWizardState;
    procedure AddButtonToPalette;
    procedure LoadSettings();
    procedure SaveSettings();
    procedure DoPopupClick(Sender: TObject);
    procedure DoPopupChangeProfile(Sender: TObject);
    procedure UpdatePopupMenu;
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Execute;
    procedure Modified;

    function GetIDString: string;
    function GetName: string;

    constructor Create;
    destructor Destroy; override;
    procedure ConfigureClick(Sender: TObject);
  end;

  TIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    FNotifierList : TStrings;
    constructor Create;
    destructor Destroy; override;
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject;
      var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
  end;

  TFormNotifier = class(TNotifierObject, IOTANotifier, IOTAFormNotifier)
  private
    FEditor             : IOTAFormEditor;
    FInsertedComponents : TStrings;
    FIDENotifier        : TIdeNotifier;
    FRenameTimer        : TTimer;
    constructor Create(Editor : IOTAFormEditor; IDENotify : TIdeNotifier);
    destructor Destroy; override;
    procedure RenameComponents(Sender: TObject);

    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle;
      const OldName, NewName: string);
    procedure ProcessName(var Component : IOTAComponent;
                          var Group : TCNAComponentGroup;
                          var Compo : TCNAComponent);
    procedure ProcessProperties(var Component : IOTAComponent;
                                var Group : TCNAComponentGroup;
                                var NoPA : Boolean;
                                var PAResult : TModalResult);
  end;

var
  NotifierIndex: Integer;
  ObjList : TList;
  Wizard : TWizardCNA;
  ComponentsPath : String;


const
  MaxSet = 255;       // Largest ordinal value in a Delphi set.
  BitsPerByte = 8;
  // Mask to force the minimum set value to be
  // a set element on a byte boundary.
  ByteBoundaryMask = not (BitsPerByte - 1);

type
  TSet = set of 0..MaxSet;

{$Region 'Register and Unregister Notifiers'}
procedure Register;
var
  Services: IOTAServices;
begin
  Wizard:=TWizardCNA.Create;
  RegisterPackageWizard(Wizard);
  Services := BorlandIDEServices as IOTAServices;
  Assert(Assigned(Services), 'IOTAServices not available');
  NotifierIndex := Services.AddNotifier(TIdeNotifier.Create);

end;

procedure RemoveNotifier;
var
  Services: IOTAServices;
begin
  if NotifierIndex <> -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    Assert(Assigned(Services), 'IOTAServices not available');
    Services.RemoveNotifier(NotifierIndex);
  end;
end;
{$EndRegion}

{$Region 'HelperFunctions'}
function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;

function OrdToString(Info: PTypeInfo; Value: Integer): string;
resourcestring
  sCvtError = 'OrdToString: type kind must be ordinal, not %s';
const
  AsciiChars = [32..127]; // printable ASCII characters
begin
  case Info.Kind of
  // In D5, the Value must fit in an Integer, for tkInt64 is not
  // likely to occur. A future version of Delphi might define
  // Int64 so it fits in an Integer, so for future expansion,
  // handle the case of tkInt64.
  tkInteger, tkInt64:
    Result := IntToStr(Value);
  tkChar, tkWChar:
    if Value in AsciiChars then
      Result := '''' + Chr(Value) + ''''
    else
      Result := Format('#%d', [Value]);
  tkEnumeration:
    Result := GetEnumName(Info, Value);
  else
    raise EConvertError.CreateFmt(sCvtError, [GetEnumName(TypeInfo(TTypeKind), Ord(Info.Kind))]);
  end;
end;


function MySetToString(Info: PTypeInfo; {const Value;} const Separator, Prefix, Suffix: string): string; overload;
var
  CompInfo: PTypeInfo;           // Type info for the set's component type
  CompData: PTypeData;           // Type data for CompInfo.
 /// SetValue: TSet absolute Value; // The set value as a convenient set type.
  Element: 0..MaxSet;            // A member of the set.
begin
  CompInfo := GetTypeData(Info)^.CompType^;
  CompData := GetTypeData(CompInfo);

  Result := '';
  for Element := CompData.MinValue to CompData.MaxValue do
  begin
    //if (Element - MinElement) in SetValue then
      if Result = '' then
        Result := Prefix + OrdToString(CompInfo, Element)+'='+IntToStr(Trunc(Power(2,Element)))
      else
         Result := Result + Separator + OrdToString(CompInfo, Element)+'='+IntToStr(Trunc(Power(2,Element)));
  end;
  if Result = '' then
    Result := Prefix + Suffix
  else
    Result := Result + Suffix;
end;

function MyEnumToString(Info: PTypeInfo;  const Separator, Prefix, Suffix: string): string; overload;
const
  MaxEnumValues = 255;
var
  TypeData: PTypeData;           // Type data for CompInfo.
  Element: 0..MaxEnumValues;            // A member of the set.
begin
  TypeData := GetTypeData(Info);

  Result := '';
  for Element := TypeData.MinValue to TypeData.MaxValue do
  begin
    if Result = '' then
      Result := Prefix + OrdToString(Info, Element)+'='+IntToStr(Element)
    else
      Result := Result + Separator + OrdToString(Info, Element)+'='+IntToStr(Element);
  end;
  if Result = '' then
    Result := Prefix + Suffix
  else
    Result := Result + Suffix;
end;

procedure ListProperties(AInstance: TPersistent; AList: TStrings);
var
  i,j:        integer;
  pInfo:    PTypeInfo;
  pType:    PTypeData;
  propList: PPropList;
  propCnt:  integer;
  tmpStr:   TStrings;
  PropValue : Variant;
  S         : String;
begin
  pInfo := AInstance.ClassInfo;

  if (pInfo = nil) or (pInfo^.Kind <> tkClass) then
    exit;
//    raise Exception.Create('Invalid type information');
  pType := GetTypeData(pInfo); // Pointer to TTypeData.
  // If any properties, add them to the list.
  propCnt := pType^.PropCount;

  if propCnt > 0 then
  begin
    // Get memory for the property list.
    //
    GetMem(propList, sizeOf(PPropInfo) * propCnt);
    try
      // Fill in the property list.
      GetPropInfos(pInfo, propList);
      // Fill in info for each property.
      for i := 0 to propCnt - 1 do
      begin
        if  not (propList[i].PropType^.Kind in [tkMethod,tkUnknown]) then
        begin
          try
          PropValue:=GetPropValue(AInstance,propList[i]);
          except
            on E:Exception do
            begin
              MsgServices.AddTitleMessage(Format(GetLangString('MSG_SkippedProperty'),
                                          [AInstance.ClassType.ClassName,propList[i].Name,E.Message]));
              continue;
            end;
          end;

          if (propList[i].PropType^.Kind = tkClass) and (PropValue = 0) then
            Continue;

          s:=propList[i].Name+'=';//'|'+OrdToString(TypeInfo(TTypeKind),Integer(propList[i].PropType^.Kind))+'=';

          if (propList[i].PropType^.Kind = tkSet) then
            s:=s+MySetToString(propList[i].PropType^,',','[',']');
          if (propList[i].PropType^.Kind = tkEnumeration) then
            s:=s+MyEnumToString(propList[i].PropType^,',','[',']');


          if (propList[i].PropType^.Kind = tkClass) then
          begin
            if (ObjList.IndexOf(Pointer(Integer(PropValue)))<0) then
            begin
              ObjList.Add(Pointer(Integer(PropValue)));
              tmpStr:=TStringList.Create;
              try
                ListProperties(TPersistent(Integer(PropValue)),tmpStr);
              except
                MsgServices.AddTitleMessage(Format(GetLangString('MSG_EGettingPropList'),[propList[i].PropType^.Name]));
              end;
              for j:=0 to tmpStr.count-1 do
                AList.AddObject(propList[i].Name+'.'+tmpStr[j],Tmpstr.Objects[j]);
              tmpStr.Free;
            end;
          end
          else
            AList.AddObject(s,TObject(propList[i].PropType^.Kind));
        end;
      end;
    finally
      FreeMem(propList, sizeOf(PPropInfo) * propCnt);
    end;
  end;
end;

procedure FindProperties(ClassName : String; var Properties : TStrings);
var
  AClass: TComponentClass;
  obj: TObject;
  MStrings : TStrings;
  CreateFromClass : Boolean;
  i : Integer;
  FileName : String;
begin
  FileName:=ComponentsPath+ClassName+'.cna';
  MStrings:=TStringList.Create;
  AClass:=nil;
  try
    ObjList:=TList.Create;
    try
      AClass := TComponentClass(FindClass(ClassName));
      CreateFromClass:=true;
    except
      CreateFromClass:=false;
      MsgServices.AddTitleMessage(Format(GetLangString('MSG_DemandLoad'),[ClassName]));
    end;

    if (CreateFromClass) then
    begin
      try
        obj := AClass.Create(nil);
      except
        on E:Exception do
        begin
          obj:=nil;
          MsgServices.AddTitleMessage(Format(GetLangString('MSG_ObjectNotCreated'),[E.Message]));
        end;
      end;
      if (obj<>nil) and (obj.ClassType.InheritsFrom(TComponent)) then
      begin
        ObjList.Add(Pointer(obj));
        ListProperties(TPersistent(obj),Properties);
      end;
      MStrings.AddStrings(Properties);
      for i:=0 to Properties.Count-1 do
        MStrings[i]:=Format('%.2d',[Integer(Properties.Objects[i])])+MStrings[i];
      if (FileExists(FileName)) then
        SetFileAttributes(PChar(FileName),0);
      MStrings.SaveToFile(FileName);
      SetFileAttributes(PChar(FileName),FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
    end
    else
    begin
      try
        if (FileExists(FileName)) then
        begin
          Properties.LoadFromFile(FileName);
          for i:=0 to Properties.Count-1 do
          begin
            Properties.Objects[i]:=TObject(StrToInt(Copy(Properties[i],1,2)));
            Properties[i]:=Copy(Properties[i],3,Length(Properties[i])-2);
          end;
        end
        else
          MsgServices.AddTitleMessage(Format(GetLangString('MSG_FileLoad'),[ClassName,FileName]));
      except
        on E:Exception do
          MsgServices.AddTitleMessage(E.Message);
      end;
    end;

  finally
    MStrings.Free;
    ObjList.Free;
    if Assigned(Obj) then
      FreeAndNil(Obj);
  end;
end;

{$EndRegion}

{$Region 'TIDENotifier'}
  {$Region 'These Methods remain empty'}
procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;


{$EndRegion}

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  i         : integer;
  services  : IOTAModuleServices;
  module    : IOTAModule;
  edit      : IOTAEditor;
  Notifier  : TFormNotifier;
begin
  if NotifyCode=ofnFileOpened then
  begin
    Services := BorlandIDEServices as IOTAModuleServices;

    Module:=services.FindModule(Filename);

    if module<>nil then
      for I := 0 to module.ModuleFileCount - 1 do
      begin
        if Supports(Module.ModuleFileEditors[i], IOTAFormEditor, Edit) then
          break; 
      end;


      //edit:=Module.ModuleFileEditors[1{1=dfm oder 0=pas}];

    if edit<>nil then
    begin
      //MsgServices.AddTitleMessage('Notify established '+edit.FileName);
      Notifier:=TFormNotifier.Create((Edit as IOTAFormEditor), Self);
      FNotifierList.AddObject(IntToStr(edit.AddNotifier(Notifier)), TObject(Edit));
    end;
  end;

  if NotifyCode=ofnFileClosing then
  begin
    Services := BorlandIDEServices as IOTAModuleServices;

    Module:=services.FindModule(Filename);

    Edit:=nil;

    if module<>nil then
      for I := 0 to module.ModuleFileCount - 1 do
      begin
        if Supports(Module.ModuleFileEditors[i], IOTAFormEditor, Edit) then
          break;
      end;

    //if module<>nil then
    //  edit:=Module.ModuleFileEditors[1{1=dfm oder 0=pas}];

    if edit<>nil then
    begin
      i:=FNotifierList.IndexOfObject(TObject(Edit));
      if i>=0 then
      begin
        edit.RemoveNotifier(StrToInt(FNotifierList[i]));
        FNotifierList.Delete(FNotifierList.IndexOfObject(TObject(Edit)));
        //MsgServices.AddTitleMessage('Notify released '+edit.FileName);
      end;
    end;
  end;
        
end;

constructor TIdeNotifier.Create;
begin
  inherited;
  FNotifierList:=TStringlist.Create;
end;

destructor TIdeNotifier.Destroy;
begin
  FNotifierList.Free;
  inherited;
end;

{$EndRegion}

{$Region 'TFormNotifier'}
  {$Region 'These Methods remain empty'}
procedure TFormNotifier.FormSaving;
begin

end;

procedure TFormNotifier.FormActivated;
begin
end;
  {$EndRegion}


constructor TFormNotifier.Create(Editor : IOTAFormEditor; IDENotify : TIdeNotifier);
begin
  inherited Create;
  FEditor:=Editor;
  FInsertedComponents:=TStringList.Create;
  FRenameTimer:=TTimer.Create(nil);
  FRenameTimer.Enabled:=False;
  FRenameTimer.OnTimer:=RenameComponents;
  FRenameTimer.Interval:=20;

  FIDENotifier:=IDENotify;
end;

destructor TFormNotifier.Destroy;
begin
  FInsertedComponents.Free;
  FRenameTimer.Free;
  inherited;
end;

procedure TFormNotifier.ComponentRenamed(ComponentHandle: TOTAHandle;
  const OldName, NewName: string);
begin
  if (Wizard.FSettings.ExpertActive) then
  begin
    if (OldName='') and (NewName<>'') then
    begin
      FRenameTimer.Enabled:=false;
      FInsertedComponents.AddObject(NewName,ComponentHandle);
      FRenameTimer.Enabled:=true;
    end;
  end;
end;

procedure TFormNotifier.ProcessName(var Component : IOTAComponent;
                      var Group : TCNAComponentGroup;
                      var Compo : TCNAComponent);
var
  NewName,
  CurName : String;
  CurNameWide : WideString;
  NameIsWide : Boolean;

  ShowDlg : Boolean;
  PrePart,
  SufPart : String;

  idxName : Integer;
  formName  : TformName;

  SelFrom,
  SelLength : Integer;

  AllOk : Boolean;
begin
  //componentnames in .net are widestrings, take care
  NameIsWide:=false;
  case Component.GetPropTypeByName('Name') of
    tkWString :
    begin
      NameIsWide:=true;
      Component.GetPropValueByName('Name', CurNameWide);
      CurName:=CurNameWide;
    end;
  else
    Component.GetPropValueByName('Name',CurName);
  end;

  //should we do something with the name?
  if (Wizard.FSettings.UseNA) then
  begin
    //by default we do not show the dialog
    ShowDlg:=false;
    if (Group<>nil) then
    begin //we found a group? nice to know
      //lets build the entire prefix and suffix
      PrePart:=Group.Prefix+IfThen((Trim(Group.Prefix)<>''),Wizard.Fsettings.NADelimiter);
      SufPart:=IfThen((Trim(Group.Suffix)<>''),Wizard.Fsettings.NADelimiter)+Group.Suffix;

      //the whole name is ...
      NewName:=PrePart+CurName+SufPart;

      //selection stuff for the dialog
      SelFrom:=Length(PrePart);
      SelLength:=Length(CurName);

      if (Wizard.FSettings.NACreateName) then
      begin//oh, nice, we have to build a unique name for this component
        idxName:=0;
        repeat
          Inc(idxName);
          NewName:=PrePart+Compo.ObjectName+IntToStr(idxName)+
                SufPart;
          //we do this until there is no component with this name
        until FEditor.FindComponent(NewName)=nil;
        SelLength:=Length(Compo.ObjectName+IntToStr(idxName));
      end; //if (Wizard.FSettings.NACreateName)

      //check if the user wishes to see a dialog for components without prefix/suffix
      if ((Trim(Group.Prefix)='') and (Trim(Group.Suffix)='')) and
          Wizard.FSettings.NAPopupIfAssigned then
        ShowDlg:=false
      else
        ShowDlg:=true;
    end //if (Group<>nil)
    else if (not Wizard.FSettings.NAPopupIfAssigned) then
    begin
      //a component without settings(group) and the user wishes to see the dialog every time
      NewName:=CurName;
      SelFrom:=0;
      SelLength:=Length(NewName);
      ShowDlg:=true;
    end;

    if (ShowDlg) then
    begin
      //the time has come ... the dialog has to be created
      try
        formName:=TformName.Create(nil);
        formName.SelFrom:=SelFrom;
        formName.SelLength:=SelLength;
        repeat
          formName.edName.Text:=NewName;
          AllOK:=true;
          if (formName.ShowModal<>mrOK) then
            //the cancel-button/esc tells us, that the name should not be changed
            break
          else
          begin
            try
              if (NameIsWide) then
              begin
                CurNameWide:=WideFormat('%s',[formName.edName.Text]);
                Component.SetPropByName('Name',CurNameWide);
              end
              else
              begin
                NewName:=formName.edName.Text;
                Component.SetPropByName('Name',NewName);
              end;
            except
              on E:Exception do
              begin
                MessageDlg(E.Message, mtError, [mbOK], 0);
                AllOK:=false;
              end;
            end; //try ... except
          end; //if (formName.ShowModal<>mrOK) ... else
        until AllOK;
      finally
        formName.Release;
        FreeAndNil(formName);
      end; //try ... finally
    end; //if (ShowDlg)
  end; // if (Wizard.FSettings.UseNA)
end;


procedure TFormNotifier.ProcessProperties(var Component: IOTAComponent;
                                          var Group: TCNAComponentGroup;
                                          var NoPA : Boolean;
                                          var PAResult : TModalResult);
var
  slProp : TStringList;
  idxProp,
  idxPropPart : Integer;
  PropName : String;
  Prop : IOTAComponent;

  myForm : TFormCNAPropEdBase;

  PropString  : String;
  PropInt     : Integer;
  PropInt64   : Int64;
  PropFloat   : Extended;
  PropChar    : Char;
  PropWideStr : WideString;

  NewVal : String;
  ShowInputBox : Boolean;

  PrePart,
  SufPart : String;
  idxChar : Word;

  ComponentName : String;
begin
  //now we handle the properties
  if (Wizard.FSettings.UsePA) and (Group<>nil) and (not NoPA) then
  begin
    if (Group.PropertiesSet.Count>0) and (Wizard.FSettings.PAShowDLG) then
    begin
      //by default "PAResult"=mrNone; in this case ask the user
      if PAResult=mrNone then
        PAResult:=MessageDlg(GetLangString('PABox_ShouldChange'), mtConfirmation, [mbYes, mbNo], 0);

      if (PAResult = mrNo) then
      begin
        NoPA:=true;
        exit;
      end;
    end;
    try
      slProp:=TStringList.Create;
      slProp.Delimiter:='.';
      for idxProp:=0 to Group.PropertiesSet.Count-1 do
      begin
         PropString:=Group.PropertiesSet.ValueFromIndex[idxProp];
        //now find the property-parent out of the property-string
        //example: Font.Name ... we have to find the Font as an IOTAComponent
        slProp.DelimitedText:=Group.PropertiesSet.Names[idxProp];
        Prop:=Component;
        for idxPropPart:=0 to slProp.Count-2 do
        begin
          if (Prop<>nil) then
            if not Prop.GetPropValueByName(slProp[idxPropPart],Prop) then
              Prop:=nil;
        end;
        PropName:=slProp[slProp.Count-1];

        if (Prop<>nil) then
        //isnt it funny? we found a property! :)
        begin
          //should we bring up a dialog?
          if (Integer(Group.PropertiesSet.Objects[idxProp]) and 1) = 1 then
          begin
            case TTypeKind(Group.PropertiesAvailable.Objects[Group.PropertiesAvailable.IndexOfName(Group.PropertiesSet.Names[idxProp])]) of
              tkEnumeration:
              begin
                myForm:=TFormCNAPropEdEnum.Create(nil);
                if Prop.GetPropValueByName(PropName,PropInt) then
                  PropString:=IntToStr(PropInt);
              end;
              tkSet:
              begin
                myForm:=TFormCNAPropEdSet.Create(nil);
                if Prop.GetPropValueByName(PropName,PropInt) then
                  PropString:=IntToStr(PropInt);
              end;
              tkInteger:
              begin
                myForm:=TFormCNAPropEdInteger.Create(nil);
                myForm.Tag:=32;
                if Prop.GetPropValueByName(PropName,PropInt) then
                  PropString:=IntToStr(PropInt);
              end;
              tkInt64:
              begin
                myForm:=TFormCNAPropEdInteger.Create(nil);
                myForm.Tag:=64;
                if Prop.GetPropValueByName(PropName,PropInt64) then
                  PropString:=IntToStr(PropInt64);
              end;
              tkString, tkLString, tkWString:
              begin
                myForm:=TFormCNAPropEdString.Create(nil);
                Prop.GetPropValueByName(PropName,PropString);
              end;
              tkFloat:
              begin
                myForm:=TFormCNAPropEdFloat.Create(nil);
                if Prop.GetPropValueByName(PropName,PropFloat) then
                  PropString:=FloatToStr(PropFloat)
              end;
              tkChar:
              begin
                myForm:=TFormCNAPropEdString.Create(nil);
                TFormCNAPropEdString(myForm).edNewValue.MaxLength:=1;
                Prop.GetPropValueByName(PropName,PropChar);
                PropString:=PropChar;
              end;
            end;
            if (Assigned(myForm)) then
            begin
              NewVal:=Group.PropertiesSet.ValueFromIndex[idxProp];
              Component.GetPropValueByName('Name',ComponentName);

              {If we want to change the caption, and the old value is empty or the name of the component then
              propose the name of the component as value}
              if (AnsiSameText(PropName,'caption') and
                  (AnsiSameText(PropString,'') or AnsiSameStr(ComponentName,PropString))) then
              begin
                NewVal:=ComponentName;

                //Remove Pre/Suffix
                PrePart:=Group.Prefix+IfThen((Trim(Group.Prefix)<>''),Wizard.Fsettings.NADelimiter);
                SufPart:=IfThen((Trim(Group.Suffix)<>''),Wizard.Fsettings.NADelimiter)+Group.Suffix;
                if (AnsiStartsStr(PrePart,NewVal)) then
                  Delete(NewVal,1,Length(PrePart));
                if (AnsiEndsStr(SufPart,NewVal)) then
                  Delete(NewVal,Length(NewVal)-Length(SufPart),Length(SufPart));

                //Parse the String for UpperCase chars in order to insert spaces
                idxChar:=2;
                while idxChar<Length(NewVal) do
                begin
                  if (UpperCase(NewVal[idxChar])=NewVal[idxChar]) then
                  begin
                    Insert(' ',NewVal,idxChar);
                    Inc(idxChar);
                  end;
                  Inc(IdxChar);
                end;
              end;

              ShowInputBox:=false;

              MyForm.GetNewValue(PropString,
                               ' '+ComponentName+'.'+Group.PropertiesSet.Names[idxProp],
                               Group.PropertiesAvailable.ValueFromIndex[Group.PropertiesAvailable.IndexOfName(Group.PropertiesSet.Names[idxProp])],
                               true,
                               NewVal,
                               ShowInputBox);
              if (NewVal<>'<!--NILORNULL-->') and (NewVal<>'<!--CANCELOFDLG-->') then
                PropString:=NewVal;
              myForm.Release;
              FreeAndNil(MyForm);
            end;
          end;

          //lets cast the value for later use
          TryStrToInt(PropString,PropInt);
          TryStrToInt64(PropString,PropInt64);
          TryStrToFloat(PropString,PropFloat);
          if (Length(PropString)>0) then
            PropChar:=PropString[1];
          PropWideStr:=WideFormat('%s',[PropString]);

          case Prop.GetPropTypeByName(slProp[slProp.Count-1]) of
            tkEnumeration,tkSet : Prop.SetPropByName(slProp[slProp.Count-1],PropInt);
            tkInteger : Prop.SetPropByName(PropName,PropInt);
            tkInt64   : Prop.SetPropByName(PropName,PropInt64);
            tkChar    : Prop.SetPropByName(PropName,PropChar);
            tkFloat   : Prop.SetPropByName(PropName,PropFloat);
            tkLString, tkString: Prop.SetPropByName(PropName,PropString);
            tkWString : Prop.SetPropByName(PropName,PropWideStr);
          end;
        end;
      end;
    finally
      slProp.Free;
    end;
  end;
end;

procedure TFormNotifier.RenameComponents(Sender: TObject);
var
  i       : Integer;
  Component : IOTAComponent;
  Group     : TCNAComponentGroup;
  Compo     : TCNAComponent;
  idxGroups   : Integer;

  NoPA   : Boolean;
  PAResult    : TModalResult;
begin
  PAResult:=mrNone;
  FRenameTimer.Enabled:=False;
  if (Wizard.FSettings.curProfile=nil) then
  begin //no active profile selected
    MsgServices.AddTitleMessage(GetLangString('MSG_SelectProfile'));
    exit;
  end;
  NoPA:=false; //by default we want to use the property-assistant
  try
    //lets parse the StringList for inserted components
    for i:=0 to FInsertedComponents.Count-1 do
    begin

      //Component:=FEditor.GetComponentFromHandle(FInsertedComponents.Items[i]);
      Component:=FEditor.FindComponent(FInsertedComponents[i]);

      //lets find the group which holds the current component
      Group:=nil;
      for idxGroups:=0 to Wizard.FSettings.CurProfile.GetGroupCount-1 do
      begin
        Compo:=Wizard.FSettings.CurProfile.GetGroup(idxGroups).GetComponent(Component.GetComponentType);
        if (Compo<>nil) then
        begin
          Group:=Wizard.FSettings.CurProfile.GetGroup(idxGroups);
          break;
        end;
      end;

      ProcessName(Component,Group,Compo);
      ProcessProperties(Component,Group,NoPA,PAResult);
    end;
  finally
    //lets clear the list of components, because they are processed
    FInsertedComponents.Clear;
  end;
end;

{$EndRegion}

{$Region 'TWizardCNA'}

procedure TWizardCNA.Modified;
begin

end;

function TWizardCNA.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

constructor TWizardCNA.Create;
var
  G : TCNAComponentGroup;
  Regi: TRegistry;
begin
  WizardDataModule:=TCNADataModule.Create(nil);

  SplashScreenServices.AddPluginBitmap('CNA (Component Naming Assistant)',WizardDataModule.bmp.Handle,false,'Freeware without any warranty',' by TheUnknownOnes');
  SplashScreenServices.StatusMessage('CNA: Adding button to palette');
  AddButtonToPalette;

  Fsettings.Profiles:=TCNAProfiles.Create;

  ComponentsPath:='';

  SplashScreenServices.StatusMessage('CNA: Searching projects directroy');
  Regi:=TRegistry.Create;
  Regi.RootKey:=HKEY_CURRENT_USER;
  try
    if (Regi.OpenKey('Software\Borland\BDS\4.0\Environment Variables\',false)) then
      ComponentsPath:=Regi.ReadString('BDSPROJECTSDIR');
  finally
    Regi.CloseKey;
    Regi.Free;
  end;

  if (Trim(ComponentsPath)='') then
    ComponentsPath:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('BDSPROJECTSDIR'))+'CNA_Components\'
  else
    ComponentsPath:=IncludeTrailingPathDelimiter(ComponentsPath)+'CNA_Components\';
  if (not DirectoryExists(ComponentsPath)) then
    CreateDir(ComponentsPath);

  SplashScreenServices.StatusMessage('CNA: Loading settings');
  LoadSettings;
  SplashScreenServices.StatusMessage('CNA: Updating menu');
  UpdatePopupMenu;
end;

procedure TWizardCNA.AfterSave;
begin

end;

function TWizardCNA.GetIDString: string;
begin
  Result := 'TUO.CNA';
end;

procedure TWizardCNA.BeforeSave;
begin

end;

function TWizardCNA.GetName: string;
begin
  Result:='CNA';
end;

procedure TWizardCNA.Destroyed;
begin

end;

destructor TWizardCNA.Destroy;
begin
  try
    while FMIProfiles.Count>0 do
      FMIProfiles.Items[0].Free;
    while FPopUp.Items.Count>0 do
      FPopUp.Items[0].Free;
    FreeAndNil(FPopUp);

    // NOTE: we should free FButton here
    //       but if we do Delphi will crash on exit
    //FButton.Parent:=nil;
    //FreeAndNil(FButton);
    //FButton.Visible:=false; //:)
    WizardDataModule.Free;

    FSettings.Profiles.Free;
  except
    on E:Exception do
    begin
      MsgServices.AddTitleMessage(E.Message);
    end;
  end;

  inherited;
end;

procedure TWizardCNA.Execute;
begin

end;    

procedure TWizardCNA.DoPopupClick(Sender: TObject);
begin
  FPopUp.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TWizardCNA.UpdatePopupMenu;
var
  i : integer;
  mi: TMenuItem;
begin
  if (Assigned(FMIProfiles)) then
  begin
    while FMIProfiles.Count>0 do
      FMIProfiles.Items[0].Free;

    for i:=0 to FSettings.Profiles.GetProfileCount-1 do
    begin
      mi:=TMenuItem.Create(nil);
      mi.Caption:=FSettings.Profiles.GetProfile(i).ProfileName;
      mi.AutoCheck:=True;
      mi.RadioItem:=True;
      mi.OnClick:=DoPopupChangeProfile;
      mi.Tag:=Integer(FSettings.Profiles.GetProfile(i));
      if mi.Caption=FSettings.curProfile.ProfileName then
        mi.Checked:=true;

      FMIProfiles.Add(mi);
    end;
  end;
end;

procedure TWizardCNA.DoPopupChangeProfile(Sender: TObject);
begin
  FSettings.curProfile:=TCNAProfile(TMenuItem(Sender).Tag);
end;

procedure TWizardCNA.AddButtonToPalette;
var
  i, j : integer;
 xPos  : Integer;
 mi    : TMenuItem;
 FToolBar : TToolBar;
begin
  for i:=0 to Application.ComponentCount-1 do
  begin
    if Application.Components[i].Name='ToolForm' then
      FToolForm:=TForm(Application.Components[i]);
  end;

  if FToolForm<>nil then
  begin
    for i:=0 to FToolForm.ControlCount-1 do
    begin
      if FToolForm.Controls[i].ClassType = TToolBar then
        FToolBar:=TToolBar(FToolForm.Controls[i]);

      if FToolForm.Controls[i].ClassType.ClassName='TIDECategoryButtons' then
        FCatButtons := TCategoryButtons(FToolForm.Controls[i]);
    end;
  end;



  if (FToolBar<>nil) and (FCatButtons<>nil) then
  begin
    xPos:=0;
    for i:=0 to FToolBar.ControlCount-1 do
    begin
      if TControl(FToolBar.Controls[i]).Left+TControl(FToolBar.Controls[i]).Width>xPos then
          xPos:=TControl(FToolBar.Controls[i]).Left+TControl(FToolBar.Controls[i]).Width;
    end;
    
    FPopUp:=TPopupActionBar.Create(nil);
    //MenuItem for Configuration
    mi:=TMenuItem.Create(nil);
    mi.Caption:=GetLangString('miConfigure');
    mi.OnClick:=ConfigureClick;
    FPopUp.Items.Add(mi);

    mi:=TMenuItem.Create(nil);
    mi.Caption:='-';
    FPopUp.Items.Add(mi);

    FMIProfiles:=TMenuItem.Create(nil);
    FMIProfiles.Caption:=GetLangString('miProfiles');
    FPopUp.Items.Add(FMIProfiles);

    FButton:=TToolButton.Create(nil);
    FButton.ImageIndex:=FToolBar.Images.AddImage(WizardDataModule.imlButtons,0);
    //WizardDataModule.imlButtons.GetBitmap(0,FButton.Glyph);
    //FButton.Left:=xPos;
    //FButton.Top:=4;
    //FButton.Flat:=True;
    FButton.Hint:='Configure Component Naming Assistant';
    FButton.ShowHint:=True;
    FButton.Left:=FToolbar.Buttons[FToolBar.ButtonCount-1].Left+
                          FToolbar.Buttons[FToolBar.ButtonCount-1].Width;
    FButton.Parent:=FToolBar;
    //FToolBar.InsertComponent(FButton);

    FButton.OnClick:=DoPopupClick;
    WizardDataModule.miCNAConfig.OnClick:=ConfigureClick;
  end;
end;

procedure TWizardCNA.ConfigureClick(Sender: TObject);

var
  formConfig : TformConfig;
  pkgIDX, CompIDX : Integer;
  pkgServ         : IOTAPackageServices;
  SR : TSearchRec;
  FileAttrs : Integer;
  CompoName : String;
begin
  formConfig:=TFormConfig.Create(nil);

  pkgServ:=(BorlandIDEServices as IOTAPackageServices);
  for pkgIDX:=0 to pkgServ.PackageCount-1 do
  begin
    for CompIDX:=0 to pkgServ.ComponentCount[pkgIDX] do
    begin
      if trim(pkgServ.ComponentNames[pkgIDX, CompIDX])<>'' then
        formConfig.comComponents.Items.Add(pkgServ.ComponentNames[pkgIDX, CompIDX]);
    end;
  end;

  FileAttrs:= faAnyFile-faDirectory-faVolumeID-faSymLink;

  if (FindFirst(ComponentsPath+'*.cna',FileAttrs,SR)=0) then
  begin
    repeat
      if (SR.Attr and FileAttrs)=SR.Attr then
      begin
        CompoName:=StringReplace(sr.Name,ExtractFileExt(sr.Name),'',[rfIgnoreCase]);
        if (formConfig.comComponents.Items.IndexOf(CompoName)=-1) then
          formConfig.comComponents.Items.Add(CompoName);
      end;
  	until FindNext(SR)<>0;
    SysUtils.FindClose(SR);
  end;

  formConfig.LoadSettings(@FSettings);
  
  FormConfig.ShowModal;
  formConfig.Release;
  FreeAndNil(FormConfig);
  SaveSettings;
  UpdatePopupMenu;
end;

procedure TWizardCNA.LoadSettings;
var
  Ini : TIniFile;
  iProfile,
  iGroup,
  iCompo,
  iProperties : Integer;
  Profiles,
  Groups,
  Compos : TStrings;
  Profile : TCNAProfile;
  Group : TCNAComponentGroup;
  NewSettings : TStrings;
begin
  try
    Ini:=TIniFile.Create(ComponentsPath+'settings.ini');
    FSettings.ExpertActive:=Ini.ReadBool('General','ExpertActive',false);
    FSettings.UseNA:=Ini.ReadBool('General','UseNa',false);
    FSettings.NAPopupIfAssigned:=Ini.ReadBool('General','NAPopupIfAssigned',false);
    FSettings.NACreateName:=Ini.ReadBool('General','NACreateName',false);
    FSettings.UsePA:=Ini.ReadBool('General','UsePA',false);
    FSettings.PAShowDLG:=Ini.ReadBool('General','PAShowDLG',true);
    FSettings.NADelimiter:=Ini.ReadString('General','NADelimiter','_');


    Profiles:=TStringList.Create;
    Groups:=TStringList.Create;
    Compos:=TStringList.Create;

    Ini.ReadSections(Profiles);
    Ini.ReadSectionValues('Profiles',Profiles);

    for iProfile:=0 to Profiles.Count-1 do
    begin
      FSettings.Profiles.AddProfile(Profiles.ValueFromIndex[iProfile]);
      Profile:=FSettings.Profiles.GetProfile(Profiles.ValueFromIndex[iProfile]);
      if (Profile=nil) then continue;
      Groups.Clear;
      Ini.ReadSectionValues(Profiles.Names[iProfile],Groups);
      for iGroup:=0 to Groups.Count-1 do
      begin
        Profile.AddGroup(Groups.ValueFromIndex[iGroup]);
        Group:=Profile.GetGroup(Groups.ValueFromIndex[iGroup]);
        if (Group=nil) then continue;
        Group.Prefix:=Ini.ReadString(Profiles.Names[iProfile]+'_'+Groups.Names[iGroup]+'_NA',
                                      'Prefix','');
        Group.Suffix:=Ini.ReadString(Profiles.Names[iProfile]+'_'+Groups.Names[iGroup]+'_NA',
                                      'Suffix','');
        Ini.ReadSectionValues(Profiles.Names[iProfile]+'_'+Groups.Names[iGroup]+'_Properties',
                              Group.PropertiesSet);
        for iProperties:=0 to Group.PropertiesSet.Count-1 do
          Group.PropertiesSet.Objects[iProperties]:=
            TObject(Ini.ReadInteger(Profiles.Names[iProfile]+'_'+Groups.Names[iGroup]+'_PropertiesFlags',
                                    Group.PropertiesSet.Names[iProperties],
                                    0));

        Compos.Clear;
        Ini.ReadSectionValues(Profiles.Names[iProfile]+'_'+Groups.Names[iGroup],Compos);
        for iCompo:=0 to Compos.Count-1 do
        begin
          Group.AddComponent(Compos.Names[iCompo],true);
        end;
      end;
    end;
    FSettings.curProfile:=FSettings.Profiles.GetProfile(Ini.ReadString('General',
                                                                      'DefaultProfile',
                                                                      ''))
  finally
    Profiles.Free;
    Groups.Free;
    Compos.Free;
    Ini.Free;
  end;
end;

procedure TWizardCNA.SaveSettings;
var
  Ini : TIniFile;
  iProfile,
  iGroup,
  iCompo,
  iPropSet : Integer;
  Profile : TCNAProfile;
  Group : TCNAComponentGroup;
  Compo : TCNAComponent;
begin
  try
    Ini:=TIniFile.Create(ComponentsPath+'settings.ini');
    Ini.WriteBool('General','ExpertActive',FSettings.ExpertActive);
    Ini.WriteBool('General','UseNA',FSettings.UseNA);
    Ini.WriteBool('General','NAPopupIfAssigned',FSettings.NAPopupIfAssigned);
    Ini.WriteBool('General','NACreateName',FSettings.NACreateName);
    Ini.WriteString('General','NADelimiter',FSettings.NADelimiter);
    Ini.WriteBool('General','UsePA',FSettings.UsePA);
    Ini.WriteBool('General','PAShowDLG',FSettings.PAShowDLG);

    if (Fsettings.curProfile<>nil) then
      Ini.WriteString('General','DefaultProfile',Fsettings.curProfile.ProfileName)
    else
      Ini.WriteString('General','DefaultProfile',EmptyStr);

    if (Ini.SectionExists('Profiles')) then Ini.EraseSection('Profiles');

    for iProfile:=0 to FSettings.Profiles.GetProfileCount-1 do
    begin
      Profile:=FSettings.Profiles.GetProfile(iProfile);
      Ini.WriteString('Profiles','Profile_'+IntToStr(iProfile),Profile.ProfileName);

      if (Ini.SectionExists('Profile_'+IntToStr(iProfile))) then
        Ini.EraseSection('Profile_'+IntToStr(iProfile));

      for iGroup:=0 to Profile.GetGroupCount-1 do
      begin
        Group:=Profile.GetGroup(iGroup);
        Ini.WriteString('Profile_'+IntToStr(iProfile),'Group_'+IntToStr(iGroup),Group.GroupName);

        if (Ini.SectionExists('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup))) then
          Ini.EraseSection('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup));

        if (Ini.SectionExists('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_NA')) then
          Ini.EraseSection('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_NA');

        Ini.WriteString('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_NA',
                        'Prefix',Group.Prefix);
        Ini.WriteString('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_NA',
                        'Suffix',Group.Suffix);

        if (Ini.SectionExists('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_Properties')) then
          Ini.EraseSection('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_Properties');

        if (Ini.SectionExists('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_PropertiesFlags')) then
          Ini.EraseSection('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_PropertiesFlags');

        for iPropSet:=0 to Group.PropertiesSet.Count-1 do
        begin
          Ini.WriteString('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_Properties',
                          Group.PropertiesSet.Names[iPropSet],
                          Group.PropertiesSet.ValueFromIndex[iPropSet]);
          Ini.WriteInteger('Profile_'+IntToStr(iProfile)+'_Group_'+IntToStr(iGroup)+'_PropertiesFlags',
                           Group.PropertiesSet.Names[iPropSet],
                          Integer(Group.PropertiesSet.Objects[iPropSet]));
        end;
        for iCompo:=0 to Group.GetComponentCount-1 do
        begin
          Compo:=Group.GetComponent(iCompo);
          Ini.WriteString('Profile_'+IntToStr(iProfile)+'_'+'Group_'+IntToStr(iGroup),
                          Compo.CNAClassName,'');
        end;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

{$EndRegion}


initialization

finalization
  RemoveNotifier;

end.


