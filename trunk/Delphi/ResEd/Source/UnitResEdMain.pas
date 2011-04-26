//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, EditIntf, ToolsApi, StdCtrls, RESEDRegExpr, unitResourceGraphics,
  resed_unitExIcon, UnitResFile, ImgList, unitResourceJPEG, resx,
  unitResourceGIF, unitResourcePNG, UnitResourceElement, unitResourceRCData,
  ExtCtrls, Buttons, DockForm, ResEdVirtualTrees;

type
  TResType = (rtICON, rtBITMAP, rtCURSOR, rtJPEG, rtGIF, rtRCDATA, rtPNG, rtSTRING,
              rtMESSAGETABLE, rtVERSIONINFO, rtDotNetCustom, rtMANIFEST);

  //
  // TNodeType and TNodeData are the main pieces the VirtualStringTree works with.
  // unfortunately I recognized too late that using a different class in every
  // level in the tree would be much more appropriate. Now there is one record
  // which is used in every level and carries a lot of "useless" variables
  //
  // e.g.: the Resourcefile Level in the tree does not need a TResourceElement
  // variable. This has to be fixed urgently
  //
  TNodeType =  (ntResFile=0, ntResourceGroup, ntResource);
  TNodeData = record    //ToDo: Rework-> a different class for each NodeType
                NodeType     : TNodeType;  //Type of the Node in the Tree

                // Resourcefile related - This stuff is only for the first level
                ResourceFile : String;  //absolute Path to the resource file
                RelativeName : String;  //resource file Path relative to the project's base path
                ResFile      : TResourceList; //The actual ResourceFile

                // Resource Group related - This stuff is only for the second level
                Group        : String;  //Name of the Resource Group
                GroupDataType: TClass;  //Class Type of Resource Elements in the Group

                // Resource Name - This stuff is only for the third level
                ResName      : String;  //Name of the Resource (should be Widestring for DotNet I suppose)
                Resource     : TResourceElement; //The actual ResourceElement ... this is where we want to go :-)

                // For every level
                IsDotNet     : Boolean; //indicates that we are working with a resX File
              end;
  PNodeData = ^TNodeData;

  TFormWizardResEd = class(TDockableForm)
    PopupEditor: TPopupMenu;
    miNewResource: TMenuItem;
    miICON: TMenuItem;
    miBITMAP: TMenuItem;
    miCURSOR: TMenuItem;
    miGIF: TMenuItem;
    miJPEG: TMenuItem;
    miPNG: TMenuItem;
    miRCDATA: TMenuItem;
    N1: TMenuItem;
    miSaveToFile: TMenuItem;
    miRename: TMenuItem;
    miDelete: TMenuItem;
    miChange: TMenuItem;
    N2: TMenuItem;
    miCreateResourceFile: TMenuItem;
    miUserData: TMenuItem;
    miRefresh: TMenuItem;
    N3: TMenuItem;
    miStringTable: TMenuItem;
    miMessageTable: TMenuItem;
    miAddToSrc: TMenuItem;
    N4: TMenuItem;
    miDotNetCustomData: TMenuItem;
    miVersionInfo: TMenuItem;
    miResourceLanguage: TMenuItem;
    miCurrentResLang: TMenuItem;
    N5: TMenuItem;
    Sprachendernin1: TMenuItem;
    currentLanguage1: TMenuItem;
    miSetNeutral: TMenuItem;
    miManifest: TMenuItem;
    procedure miAddToSrcClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure TVNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: UnicodeString);
    procedure TVEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure miCreateResourceFileClick(Sender: TObject);
    procedure miChangeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TVGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure TVDblClick(Sender: TObject);
    procedure miRenameClick(Sender: TObject);
    procedure miSaveToFileClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure PopupEditorPopup(Sender: TObject);
    procedure CreateResourceClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TVGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: UnicodeString);
    procedure TVGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: UnicodeString);
    procedure TVGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure FormShow(Sender: TObject);
    procedure SetResLang(Sender: TObject);
  private      
    FProjectPath : String;
    FResFileList : TStrings;
    procedure TVFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure CreateTreeView;

    procedure LoadResources;
    procedure ClearResFileList;
    function CreateResource(ParentNode : PVirtualNode;
                            ResType    : TResType;
                            GroupID    : String;
                            FileName   : String): TResourceElement;
    procedure HandleResDetail(ParentNode : PVirtualNode;
                              RD         : TResourceElement;
                              ResFile    : TResourceList;
                              ResIndex   : Integer;
                              FileName   : String;
                              IsDotNet   : Boolean);
    procedure RenameResource(Node : PVirtualNode; NewName : String);
    procedure CreateLanguageMIs;
  public
    TV               : TVirtualStringTree;
    ExtDotNetSupport : Boolean;                    //Flag wheth
    ResXLib          : THandle;
    procedure ReturnResFiles(sl: TStrings);
    procedure ReturnResources(sl: TStrings; aResType: TResType);
  end;

  TDockableFormClass = Class Of TDockableForm;

  //functions for handling DotNet Resources
  // These will be loaded later on from ResXEd.dll
  TDotNetAddFileToResource = procedure (const FileName     : WideString;
                                        const FileType     : WideString;
                                        const ResourceName : WideString;
                                        const ResXName     : WideString);  stdcall;

  TDotNetRemoveFromResource = procedure(const ResourceName : WideString;
                                        const ResXName : WideString) stdcall;


const
  ResGroupNames : array [1..24] of string =
                         ('CURSOR','BITMAP','ICON','MENU','DIALOG',
                          'STRING','FONTDIR','FONT','ACCELERATOR',
                          'RCDATA','MESSAGETABLE','CURSOR',
                          'CURSOR','ICON','',
                          'VERSION','', 'DLGINCLUDE','',
                          'PLUGPLAY','VXD','ANICURSOR', 'ANIICON','MANIFEST');
  StrNoLanguage = 'Neutral [$00000000]';

var
  MainForm        : TDockableForm = nil;
  GlobalImageList : TCustomImageList = nil;
  IMGIDXResFile   : Integer = 0;
  IMGIDXResGroup  : Integer = 1;
  IMGIDXResource  : Integer = 2;
  ResNameGlob     : STring = '';

procedure CreateDockableForm(var FormVar: TDockableForm; FormClass: TDockableFormClass);
procedure FreeDockableForm(var FormVar: TDockableForm);
procedure ShowDockableForm(Form: TDockableForm);
function GetResTypeFromString(s: String): TResType;
procedure AddStringToSource(aString: String);

var
  DotNetAddFileToResource : TDotNetAddFileToResource;
  DotNetRemoveFromResource: TDotNetRemoveFromResource;

implementation

{$R *.dfm}

uses
  StrUtils, DeskUtil, unitResourceMessages, UnitResEdSLEditor,
  unitResourceDotNet, UnitResEdDotNetEditor, unitResourceVersionInfo,
  UnitResEdVIEditor, unitResourceXPManifests, UnitResEdManifestEditor;

{$Region 'Helper functions'}
procedure MarkProjectModified;
var
  ActiveProject : IOTAProject;
begin
  ActiveProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  if ActiveProject<>nil then
    ActiveProject.MarkModified;
end;

function GetGroupName(s : String): String;
var
  dummy : Integer;
begin
  if TryStrToInt(s, dummy) then
  begin
    Result:=resGroupNames[dummy];
    if Result='' then
      Result:=s;
  end
  else
    Result:=s;
end;

function CheckPersonality(Project: IOTAProject; var Personality: String; var IsDotNet: Boolean): Boolean;
begin
  Personality:=Project.Personality;

  result:=(Personality='Delphi.Personality') or
          (Personality='DelphiDotNet.Personality') or
          (Personality='CSharp.Personality') or
          (Personality='CPlusPlusBuilder.Personality');

  IsDotNet:=(Personality='DelphiDotNet.Personality') or
            (Personality='CSharp.Personality');
end;

function GetResTypeFromString(s: String): TResType;
begin
  if (AnsiSameText(S, 'BITMAP')) then
    result:=rtBITMAP
  else
  if (AnsiSameText(S, 'CURSOR')) then
    result:=rtCURSOR
  else
  if (AnsiSameText(S, 'ICON')) then
    result:=rtICON
  else
  if (AnsiSameText(S, 'JPEG')) then
    result:=rtJPEG
  else
  if (AnsiSameText(S, 'PNG')) then
    result:=rtPNG
  else
  if (AnsiSameText(S, 'GIF')) then
    result:=rtGIF
  else
  if (AnsiSameText(S, 'MESSAGE')) then
    result:=rtMESSAGETABLE
  else
  if (AnsiSameText(S, 'STRING')) then
    result:=rtSTRING
  else
    result:=rtRCDATA;
end;

function GetResTypeFromClassType(ct: TClass):TResType;
begin
  if ct = TVersionInfoResourceElement then
    result:=rtVERSIONINFO
  else
  if ct = TXPManifestResourceElement then
    result:=rtMANIFEST
  else
  if ct = TBitmapResourceElement then
    result:=rtBITMAP
  else
  if ct = TCursorGroupResourceElement then
    result:=rtCURSOR
  else
  if ct = TIconGroupResourceElement then
    result:=rtICON
  else
  if ct = TJPegResourceElement then
    result:=rtJPEG
  else
  if ct = TPngResourceElement then
    result:=rtPNG
  else
  if ct = TGifResourceElement then
    result:=rtGIF
  else
  if ct = TMessageResourceElement then
    result:=rtMESSAGETABLE
  else
  if ct = TStringResourceElement then
    result:=rtSTRING
  else
    result:=rtRCDATA;
end;

Function GetFilters(ResType : TResType):String;
begin
  result:='';
  case ResType of
    rtBITMAP : Result:='Bitmap|*.bmp';
    rtICON   : Result:='Icon|*.ico';
    rtCURSOR : Result:='Cursor|*.cur';
    rtJPEG   : Result:='Jpeg-Image|*.jpg; *.jpeg';
    rtGIF    : Result:='GIF-Image|*.gif';
    rtPNG    : Result:='Protable Network Graphics|*.png';
  else
    Result:='All Files|*.*';
  end;
end;

Function GetDefaultExtension(ResType : TResType):String;
begin
  result:='';
  case ResType of
    rtBITMAP : Result:='*.bmp';
    rtICON   : Result:='*.ico';
    rtCURSOR : Result:='*.cur';
    rtJPEG   : Result:='*.jpg';
    rtGIF    : Result:='*.gif';
    rtPNG    : Result:='*.png';
  else
    Result:='*.*';
  end;
end;

procedure TFormWizardResEd.RenameResource(Node : PVirtualNode; NewName : String);
var
  NodeData : PNodeData;
  i        : Integer;
  ResX     : IXMLRoot;
begin
  if (Node=nil) then exit;
  if (Trim(NewName)='') then exit;

  NodeData:=TV.GetNodeData(Node);
  if (NodeData.NodeType<>ntResource) then exit;

  if (not NodeData.IsDotNet) then
  begin
    if (NodeData.Resource is TStringResourceElement) then
    begin
      NodeData.Resource.ResourceName:=StringsIdToResId(NewName);
      NodeData.ResName:=ResIdToStringsId(NodeData.Resource.ResourceName);
    end
    else
    begin
      NodeData.Resource.ResourceName:=UpperCase(NewName);
      NodeData.ResName:=NodeData.Resource.ResourceName;
    end;
    NodeData:=TV.GetNodeData(Node);

    NodeData.ResFile.SaveToFile(NodeData.ResourceFile);
    MarkProjectModified;
  end
  else
  begin
    ResX:=LoadResX(NodeData.ResourceFile);

    for i := 0 to ResX.Data.Count - 1 do
      if (ResX.Data[i].Name=NodeData.ResName) then
      begin
        ResX.Data[i].Name:=NewName;
        NodeData.Resource.ResourceName:=NewName;
        NodeData.ResName:=NodeData.Resource.ResourceName;
      end;

    ResX.OwnerDocument.SaveToFile(NodeData.ResourceFile);
    MarkProjectModified;
  end;
end;

procedure TFormWizardResEd.ReturnResFiles(sl: TStrings);
var
  NodeData : PNodeData;
  Node     : PVirtualNode;
begin
  Node:=TV.GetFirst;

  while Node<>nil do
  begin
    NodeData:=TV.GetNodeData(Node);
    sl.AddObject(NodeData.ResourceFile, TObject(Node));
    Node:=TV.GetNextSibling(Node);
  end;
end;

procedure TFormWizardResEd.ReturnResources(sl: TStrings; aResType: TResType);
var
  NodeData : PNodeData;
  Node     : PVirtualNode;
  resType  : TResType;
  RD       : TTextResourceElement;
  i        : Integer;
begin
  Node:=TV.GetFirst;

  while Node<>nil do
  begin
    NodeData:=TV.GetNodeData(Node);
    if (NodeData.NodeType=ntResource) then
    begin
      resType:=GetResTypeFromClassType(NodeData.Resource.ClassType);

      if (ResType=aResType) then
        if (aResType in [rtSTRING,rtMESSAGETABLE]) then
        begin
          RD:=NodeData.Resource as TTextResourceElement;
          for i := 0 to RD.Count - 1 do
            if (RD.Strings[i]<>'') then
             sl.AddObject(IntToStr(RD.Ids[i])+' "'+RD.Strings[i]+'"',
                          TObject(RD.Ids[i]+1));
        end
        else
          sl.AddObject(NodeData.Resource.ResourceName, nil);
    end;

    Node:=TV.GetNext(Node);
  end;
end;

function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;

procedure TFormWizardResEd.SetResLang(Sender: TObject);
var
  myNode    : PVirtualNode;
  NodeData  : PNodeData;
begin
  myNode:=TV.GetFirstSelected;

  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);
    if NodeData.NodeType=ntResource then
    begin
      NodeData.Resource.ResourceLanguage:=TMenuItem(Sender).Tag;
      NodeData.Resource.CodePage:=LCIDToCodePage(TMenuItem(Sender).Tag);
    end;

    NodeData.ResFile.SaveToFile(NodeData.ResourceFile);
    MarkProjectModified;
  end;
end;

procedure AddStringToSource(aString: String);
var
  Services     : IOTAModuleServices;
  Module       : IOTAModule;
  SourceEditor : IOTASourceEditor;
  EditView     : IOTAEditView;
  s            : String;
  r, c         : Integer;
  quotechar    : char;
  ActiveProject: IOTAProject;
begin
  ActiveProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;

  if ActiveProject = nil then
    Exit;

  if (ActiveProject.Personality='CSharp.Personality') or
     (ActiveProject.Personality='CPlusPlusBuilder.Personality') then
    quotechar:='"'
  else
    quotechar:='''';

  Services := BorlandIDEServices as IOTAModuleServices;

  Module:=Services.CurrentModule;

  if Supports(Module.CurrentEditor, IOTASourceEditor, SourceEditor) then
  begin
    EditView := SourceEditor.EditViews[0];
    SourceEditor.Show;
    SourceEditor.SwitchToView(0);

    EditView.Position.InsertText(' ');
    r:=EditView.Position.Row;
    c:=EditView.Position.Column;
    EditView.Position.MoveRelative(0,-2);
    s:=EditView.Position.Read(3);
    if (AnsiStartsText(quotechar+' '+quotechar, s)) then
      s:=aString
    else
    if (AnsiStartsText(quotechar, s)) then
      s:=aString+quotechar
    else
      s:=quotechar+aString+quotechar;

    EditView.Position.MoveReal(r, C);
    EditView.Position.BackspaceDelete(1);

    EditView.Position.InsertText(s);
    FocusWindow(EditView.GetEditWindow.Form);
  end;
end;

{$EndRegion}

{$Region 'DockableForm Routines'}
procedure RegisterDockableForm(FormClass: TDockableFormClass;
  var FormVar; const FormName: string);
begin
  if @RegisterFieldAddress <> nil then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
end;

procedure UnRegisterDockableForm(var FormVar; const FormName: string);
begin
  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@FormVar);
end;

procedure ShowDockableForm(Form: TDockableForm);
begin
  if not Assigned(Form) then
    Exit;
  if not Form.Floating then
  begin
    Form.ForceShow;
    FocusWindow(Form);
  end
  else
    Form.Show;
end;

procedure CreateDockableForm(var FormVar: TDockableForm; FormClass: TDockableFormClass);
begin
  TCustomForm(FormVar) := FormClass.Create(nil);
  RegisterDockableForm(FormClass, FormVar, TCustomForm(FormVar).Name);
end;

procedure FreeDockableForm(var FormVar: TDockableForm);
begin
  if Assigned(FormVar) then
  begin
    FormVar.Hide;
    UnRegisterDockableForm(FormVar, FormVar.Name);
    {FormVar.Release;
    FormVar:=nil;  }   
    FreeAndNil(FormVar);
  end;
end;
{$EndRegion}

{$Region 'Update Procedure List'}

procedure TFormWizardResEd.ClearResFileList;
begin
  while FResFileList.Count>0 do
  begin
    try
      TResModule(FResFileList.objects[0]).Free;
    except
    end;
    FResFileList.Delete(0);
  end;
end;

function GetProjectResources(Project: IOTAProject; FileType: String): String;
var
  i       : Integer;
  ModInfo : IOTAModuleInfo;
  Editor  : IOTAEditor;
  RegExpr : TRegExpr;
  RegExpr2: TRegExpr;
  slresult: TStringList;

  prjFile : TStrings;

  procedure AnalyzeFile(FileName : String; AResult: TStrings);
  var
    ResFileNam : String;
    FilePath     : String;
    tmpResult    : String;
  begin
    if trim(FileName)<>'' then
    begin
      try
        prjFile.LoadFromFile(FileName);
      except
      end;

      RegExpr.InputString:=prjFile.Text;

      if (RegExpr.Exec) then
      repeat
        tmpResult:=RegExpr.Substitute('$1');

        //Now replace System Variables
        RegExpr2.Expression := '(%(.*?)%)';
        RegExpr2.InputString:=tmpResult;
        if (RegExpr2.Exec) then
        repeat
          tmpResult:=AnsiReplaceText(tmpResult,RegExpr2.Substitute('$1'),GetEnvironmentVariable(RegExpr2.Substitute('$2')));
        until RegExpr2.ExecNext = False;

        ResFileNam:=ExtractFileName(tmpResult);
        FilePath:=ExtractFilePath(tmpResult);
        if ExtractFileDrive(FilePath)='' then
          FilePath:=ExtractFilePath(FileName)+FilePath;
        if not DirectoryExists(FilePath) then
          FilePath:=ExtractFilePath(FileName);

        if FilePath+ResFileNam<>EmptyStr then
        begin
          AResult.Add(FilePath+ResFileNam);
        end;

      until RegExpr.ExecNext = False;
    end;
  end;

begin
  prjFile:=TStringList.Create;
  slresult:=TStringList.Create;
  slresult.Sorted:=true;
  slresult.Duplicates:=dupIgnore;

  RegExpr:=TRegExpr.Create;
  RegExpr2:=TRegExpr.Create;
  // RegExpr.ModifierI  :=True;
//  if (AnsiSameText(FileType, 'resx')) then
//  begin
//    RegExpr.Expression := '\{\$R ''{0,1}([:\.\w\\%]{1,}\.resx)''{0,1}\}'
 // end
//  else
    RegExpr.Expression := '\{\$R ''{0,1}([:\.\w\\%]{1,}\.res)''{0,1}\}';

  Result := '';

  for i := 0 to Project.GetModuleFileCount - 1 do
  begin
    Editor := Project.GetModuleFileEditor(i);
    AnalyzeFile(Editor.FileName, slresult);
  end;

  for i:= 0 to (Project.GetModuleCount - 1) do
  begin
    ModInfo := Project.GetModule(i);
    
    if (AnsiSameText(FileType, 'resx')) then
    begin
      if (AnsiSameText(ExtractFileExt(ModInfo.FileName),'.resx')) then
       slresult.Add(ModInfo.FileName);
    end
    else
    if (AnsiSameText(FileType, 'res')) then
    begin
      if (AnsiSameText(ExtractFileExt(ModInfo.FileName),'.res')) then
      begin
        slresult.Add(ModInfo.FileName);
      end
      else
        if AnsiSameText(ExtractFileExt(ModInfo.FileName),'.pas') or
           AnsiSameText(ExtractFileExt(ModInfo.FileName),'.inc') then
          AnalyzeFile(ModInfo.FileName, slresult);
    end;
  end;

  Result:=slresult.Text;

  prjFile.Free;
  slresult.Free;

  RegExpr.Free;
  RegExpr2.Free;
end;

procedure TFormWizardResEd.HandleResDetail(ParentNode : PVirtualNode;
                                          RD          : TResourceElement;
                                          ResFile     : TResourceList;
                                          ResIndex    : Integer;
                                          FileName    : String;
                                          IsDotNet    : Boolean);
var
  rt            : String;
  NodeGroup     : PVirtualNode;
  NodeResource  : PVirtualNode;
  NodeData      : PNodeData;
begin
  if ((RD.ClassType = TRCDataResourceElement) or
      (RD.ClassType = TResourceElement) or
      (RD.ClassType = TPngResourceElement) or
      (RD.ClassType = TBitmapResourceElement) or
      (RD.ClassType = TIconGroupResourceElement) or
      (RD.ClassType = TJPegResourceElement) or
      (RD.ClassType = TGIFResourceElement) or
      (RD.ClassType = TDIBResourceElement) or
      (RD.ClassType = TAnsiResourceElement) or
      (RD.ClassType = TUnicodeResourceElement) or
      (RD.ClassType = TStringResourceElement) or
      (RD.ClassType = TDotNetResourceElement) or
      (RD.ClassType = TMessageResourceElement) or
      (RD.ClassType = TCursorGroupResourceElement) or
      (RD.ClassType = TVersionInfoResourceElement) or
      (RD.ClassType = TXPManifestResourceElement))
    { and
      (GetGroupName(RD.ResourceType)<>'VERSION')} then
  begin
    rt:=RD.ResourceType;
    
    NodeGroup:=TV.GetFirstChild(ParentNode);
    while NodeGroup<>nil do
    begin
      NodeData:=TV.GetNodeData(NodeGroup);
      if NodeData.Group=rt then
        break;
      NodeGroup:=TV.GetNextSibling(NodeGroup);
    end;

    if NodeGroup=Nil then
    begin
      NodeGroup:=TV.AddChild(ParentNode);
      NodeData:=TV.GetNodeData(NodeGroup);
      NodeData.NodeType:=ntResourceGroup;
      NodeData.GroupDataType:=RD.ClassType;
      NodeData.IsDotNet:=IsDotNet;

      NodeData.Group:=rt;
    end;

    NodeResource:=TV.AddChild(NodeGroup);
    NodeData:=TV.GetNodeData(NodeResource);
    NodeData.NodeType:=ntResource;
    if (RD.ClassType = TStringResourceElement) then
      NodeData.ResName:=ResIdToStringsId(RD.ResourceName)
    else
      NodeData.ResName:=RD.ResourceName;
    NodeData.ResFile:=ResFile;
    NodeData.Resource:=RD;
    NodeData.ResourceFile:=FileName;
    NodeData.IsDotNet:=IsDotNet;
  end;
end;

procedure TFormWizardResEd.LoadResources;
var
  ActiveProject : IOTAProject;
  Resources     : TStringList;
  NodeData      : PNodeData;
  NodeResFile   : PVirtualNode;
  i, j          : integer;
  s             : String;
  ResFile       : TResourceList;
  perso         : String;
  isDotNet      : Boolean;
  ResX          : IXMLRoot;
  ResDet        : TDotNetResourceElement;
begin
  ClearResFileList;
  TV.Clear;

  ActiveProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;

  if ActiveProject= nil then
    exit;

  if not CheckPersonality(ActiveProject,perso,isDotNet) then
    raise Exception.Create(perso+' not supported by current version of ResEd');

  FProjectPath := ExtractFilePath(ActiveProject.FileName);

  if (isDotNet) then
  begin
  {$Region 'Load XResFiles'}
    Resources:=TStringList.Create;
    Resources.Text:=GetProjectResources(ActiveProject,'resx');
    for i:=0 to Resources.Count-1 do
    begin
      NodeResFile:=TV.AddChild(nil);
      NodeData:=TV.GetNodeData(NodeResFile);
      NodeData.NodeType:=ntResFile;
      NodeData.ResourceFile:=Resources[i];
      NodeData.RelativeName:=AnsiReplaceText(Resources[i],FProjectPath,'');
      NodeData.IsDotNet:=isDotNet;

      ResFile:=TResModule.Create;
      if FileExists(Resources[i]) then
      try
        ResX:=LoadResX(Resources[i]);

        for j := 0 to ResX.Data.Count - 1 do
        begin
          s:=ResX.Data[j].Type_+',';
          if s=',' then
            s:='System.String,';
          s:=copy(s,1,Pos(',',s)-1);

          ResDet:=TDotNetResourceElement.CreateNew(ResFile,
                                                   0,
                                                   ResX.Data[j].Name,
                                                   s);
          ResDet.rType:=s;   
          ResDet.Comment:=ResX.Data[j].Comment;
          ResDet.MimeType:=ResX.Data[j].Mimetype;
          ResDet.Text:=ResX.Data[j].Value;
        end;
      except
      end;

      FResFileList.AddObject(Resources[i],ResFile);
      NodeData.ResFile:=ResFile;

      for j:=0 to ResFile.ResourceCount-1 do
        HandleResDetail(NodeResFile,ResFile.ResourceElement[j],ResFile,j, NodeData.ResourceFile, isDotNet);
    end;
  {$EndRegion}
  end
  else
  begin
    {$Region 'Load ResFiles'}
    Resources:=TStringList.Create;
    Resources.Text:=GetProjectResources(ActiveProject,'res');

    for i:=0 to Resources.Count-1 do
    begin
      NodeResFile:=TV.AddChild(nil);
      NodeData:=TV.GetNodeData(NodeResFile);
      NodeData.NodeType:=ntResFile;
      NodeData.ResourceFile:=Resources[i];
      NodeData.RelativeName:=AnsiReplaceText(Resources[i],FProjectPath,'');
      NodeData.IsDotNet:=isDotNet;

      ResFile:=TResModule.Create;
      if FileExists(Resources[i]) then
      try
        ResFile.LoadFromFile(Resources[i]);
      except
      end;

      FResFileList.AddObject(Resources[i],ResFile);
      NodeData.ResFile:=ResFile;

      for j:=0 to ResFile.ResourceCount-1 do
        HandleResDetail(NodeResFile,ResFile.ResourceElement[j],ResFile,j, NodeData.ResourceFile, isDotNet);
    end;
    Resources.Free;
    {$EndRegion}
  end;
end;

{$EndRegion}

{$Region 'Treeview behaviour'}

procedure TFormWizardResEd.TVGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:=SizeOf(TNodeData);
end;

procedure TFormWizardResEd.TVGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData : PNodeData;
begin
  NodeData:=TV.GetNodeData(Node);
  if NodeData.NodeType=ntResFile then
    ImageIndex:=IMGIDXResFile
  else
  if NodeData.NodeType=ntResourceGroup then
    ImageIndex:=IMGIDXResGroup
  else
  if NodeData.NodeType=ntResource then
    ImageIndex:=IMGIDXResource;
end;

procedure TFormWizardResEd.TVGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
var
  NodeData : PNodeData;
begin
  NodeData:=TV.GetNodeData(Node);

  case NodeData.NodeType of
    ntResFile       : CellText:=NodeData.RelativeName;
    ntResourceGroup : CellText:=GetGroupName(NodeData.Group);
    ntResource      : CellText:=NodeData.ResName;
  end;
end;

procedure TFormWizardResEd.TVGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: UnicodeString);
var
  NodeData : PNodeData;
begin
  NodeData:=TV.GetNodeData(Node);

  case NodeData.NodeType of
    ntResFile       : HintText:=NodeData.ResourceFile;
    ntResourceGroup : HintText:=GetGroupName(NodeData.Group);
    ntResource      : HintText:=NodeData.ResName;
  end;
end;

procedure TFormWizardResEd.TVDblClick(Sender: TObject);
begin
  miChange.Click;
end;

procedure TFormWizardResEd.TVEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  NodeData : PNodeData;
begin
  NodeData:=Tv.GetNodeData(Node);
  if (NodeData.NodeType=ntResource) then
    Allowed:=true
  else
    Allowed:=false;
end;

procedure TFormWizardResEd.TVNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: UnicodeString);
begin
  RenameResource(Node, NewText);
end;

procedure TFormWizardResEd.TVFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
//var
 // NodeData : PNodeData;
begin
 { NodeData:=Tv.GetNodeData(Node);
  if (NodeData<>Nil) then
    if Assigned(NodeData.ResFile) then
      try
        FreeAndNil(NodeData.ResFile);
      except
      end;    }
end;

{$endregion}

{$Region 'Form behaviour'}
procedure TFormWizardResEd.FormShow(Sender: TObject);
begin
  LoadResources;
end;

procedure TFormWizardResEd.CreateTreeView;
begin
  TV := TVirtualStringTree.Create(Self);

  TV.Name := 'TV';
  TV.Parent := Self;
  TV.Align := alClient;
  TV.EditDelay := 500;
  TV.Header.AutoSizeIndex := 0;
  TV.Header.Font.Charset := DEFAULT_CHARSET;
  TV.Header.Font.Color := clWindowText;
  TV.Header.Font.Height := -11;
  TV.Header.Font.Name := 'Tahoma';
  TV.Header.Font.Style := [];
  TV.Header.MainColumn := -1;
  TV.Header.Options := [hoColumnResize, hoDrag];
  TV.HintMode := hmHint;
  TV.ParentShowHint := False;
  TV.PopupMenu := PopupEditor;
  TV.ShowHint := True;
  TV.TabOrder := 0;
  TV.TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
  TV.OnEditing := TVEditing;
  TV.OnDblClick := TVDblClick;
  TV.OnGetText := TVGetText;
  TV.OnGetImageIndex := TVGetImageIndex;
  TV.OnGetHint := TVGetHint;
  TV.OnGetNodeDataSize := TVGetNodeDataSize;
  TV.OnNewText := TVNewText;
  //TV.OnFreeNode := TVFreeNode;
  TV.BorderStyle := bsNone;
  TV.BevelKind := bkSoft;
  TV.BevelInner := bvLowered;
  TV.BevelOuter := bvNone;
end;

procedure TFormWizardResEd.CreateLanguageMIs;
var
  mi : TMenuItem;
  i  : Integer;
  sl : TStrings;

  AlphaEntry: TMenuItem;

  function GetAlphaEntry(s: String): TMenuItem;
  var
    idx : Integer;
    FirstLetter : String;
    subItem : TMenuItem;
  begin
    Result:=Nil;
    FirstLetter:='&'+String(s+' ')[1];
    for idx := 0 to miResourceLanguage.Count - 1 do
    begin
      if AnsiSameText(FirstLetter,miResourceLanguage.Items[idx].Caption) then
      begin
        result:=miResourceLanguage.Items[idx];
        break;
      end;
    end;

    if not Assigned(Result) then
    begin
      Result:=TMenuItem.Create(self);
      miResourceLanguage.Add(Result);
      Result.Caption:=FirstLetter;
    end;
  end;

begin
  sl:=TStringList.Create;

  for I := 0 to Languages.Count - 1 do
  begin
    sl.AddObject(Languages.Name[i]+' ['+Languages.ID[i]+']', TObject(Languages.LocaleID[i]));
  end;

  TStringList(sl).Sort;

  for I := 0 to sl.Count - 1 do
  begin
    mi:=TMenuItem.Create(self);
    GetAlphaEntry(sl[i]).Add(mi);
    mi.Tag:=Integer(sl.Objects[i]);
    mi.Caption:=sl[i];
    mi.OnClick:=SetResLang;
  end;
  sl.Free;
end;

procedure TFormWizardResEd.FormCreate(Sender: TObject);
begin
  inherited;
  FResFileList:=TStringList.Create;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;

  CreateLanguageMIs;
  CreateTreeView;
  TV.Images:=GlobalImageList;

  ExtDotNetSupport:=False;

  ResXLib:=LoadLibrary('ResXEd.dll');

  if ResXLib<>0 then
  begin
    @DotNetAddFileToResource:=GetProcAddress(ResXLib, 'AddFileToResource');
    @DotNetRemoveFromResource:=GetProcAddress(ResXLib, 'RemoveFromResource');
    ExtDotNetSupport:=True;
  end
  else
    ExtDotNetSupport:=False;
end;

procedure TFormWizardResEd.FormDestroy(Sender: TObject);
begin
  if ResXLib<>0 then
    FreeLibrary(ResXLib);

  SaveStateNecessary := True;

  TV.Free;
  FResFileList.Free;
  inherited;
end;

procedure TFormWizardResEd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeDockableForm(MainForm);
end;

{$EndRegion}

{$Region 'Resource Editor'}

procedure EditStringResource(sr : TTextResourceElement);
var
  myForm : TFormResEdSLEditor;
begin
  myForm:=TFormResEdSLEditor.Create(nil);
  try
    myForm.RD:=sr;
    myForm.EditTextResource;
  finally
    myForm.Release;
  end;
end;

procedure EditVersionInfoResource(vir: TVersionInfoResourceElement);
var
  myForm: TFormResEdVIEditor;
begin
  myForm:=TFormResEdVIEditor.Create(nil);
  try
    myForm.RD:=vir;
    myForm.EditVersionInfoResource;
  finally
    myForm.Release;
  end;
end;

procedure EditManifestResource(mar: TXPManifestResourceElement);
var
  myForm: TFormResEdManifestEditor;
begin
  myForm:=TFormResEdManifestEditor.Create(nil);
  try
    myForm.RD:=mar;
    myForm.EditManifestResource;
  finally
    myForm.Release;
  end;
end;

{$EndRegion}

{$Region 'Resource Creation/Update'}

procedure TFormWizardResEd.miChangeClick(Sender: TObject);
var
  myNode   : PVirtualNode;
  NodeData : PNodeData;
  LoadDlg  : TOpenDialog;
  resType  : TResType;
  ResX     : IXMLRoot;
  i        : Integer;
  rdDNet   : TDotNetResourceElement;
  Handled  : Boolean;
  ss : TStringStream;
begin
  myNode:=TV.GetFirstSelected;

  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);
    if NodeData.NodeType=ntResource then
      if (not NodeData.IsDotNet) then
      begin
        resType:=GetResTypeFromClassType(NodeData.Resource.ClassType);

        LoadDlg:=TOpenDialog.Create(nil);
        LoadDlg.Filter:=GetFilters(resType);
        LoadDlg.DefaultExt:=GetDefaultExtension(resType);
        try
          if (ResType in [rtMESSAGETABLE, rtSTRING, rtVERSIONINFO, rtMANIFEST])
             or LoadDlg.Execute then
          begin
            case resType of
              rtBitmap  : (NodeData.Resource as TBitmapResourceElement).LoadImage(LoadDlg.FileName);
              rtCURSOR  : (NodeData.Resource as TCursorGroupResourceElement).LoadImage(LoadDlg.FileName);
              rtICON    : (NodeData.Resource as TIconGroupResourceElement).LoadImage(LoadDlg.FileName);
              rtJPEG    : (NodeData.Resource as TJPegResourceElement).Data.LoadFromFile(LoadDlg.FileName);
              rtPNG     : (NodeData.Resource as TPngResourceElement).Data.LoadFromFile(LoadDlg.FileName);
              rtGIF     : (NodeData.Resource as TGifResourceElement).Data.LoadFromFile(LoadDlg.FileName);
              rtRCDATA  : NodeData.Resource.Data.LoadFromFile(LoadDlg.FileName);
              rtMESSAGETABLE,
              rtSTRING  : EditStringResource(NodeData.Resource as TTextResourceElement);
              rtVERSIONINFO : EditVersionInfoResource(NodeData.Resource as TVersionInfoResourceElement);
              rtMANIFEST : EditManifestResource(NodeData.Resource as TXPManifestResourceElement);
            end;

            if (not NodeData.IsDotNet) then
              NodeData.ResFile.SaveToFile(NodeData.ResourceFile);
            MarkProjectModified;
          end;
        finally
          LoadDlg.Free;
        end;
      end
      else
      begin
        rdDNet:=TDotNetResourceElement(NodeData.Resource);
        Handled:=False;
        if ExtDotNetSupport then
        begin
          try
            LoadDlg:=TOpenDialog.Create(nil);
            if AnsiSameText('System.Drawing.Bitmap',rdDNet.rType) then
            begin
              LoadDlg.Filter:=GetFilters(rtBITMAP)+'|'+
                              GetFilters(rtGIF)+'|'+
                              GetFilters(rtJPEG)+'|'+
                              GetFilters(rtPNG)+'|'+
                              GetFilters(rtRCDATA);
              LoadDlg.DefaultExt:=GetDefaultExtension(rtBITMAP);
              Handled:=True;
              if LoadDlg.Execute then
                DotNetAddFileToResource(LoadDlg.FileName, 'BITMAP', rdDNet.ResourceName, NodeData.ResourceFile);
            end
            else
            if AnsiSameText('System.Drawing.Icon',rdDNet.rType) then
            begin
              LoadDlg.Filter:=GetFilters(rtICON)+'|'+
                              GetFilters(rtCURSOR)+'|'+
                              GetFilters(rtRCDATA);
              LoadDlg.DefaultExt:=GetDefaultExtension(rtBITMAP);
              Handled:=True;
              if LoadDlg.Execute then
                DotNetAddFileToResource(LoadDlg.FileName, 'ICON', rdDNet.ResourceName, NodeData.ResourceFile);
            end
            else
            if AnsiSameText('System.Byte[]',rdDNet.rType) then
            begin
              LoadDlg.Filter:=GetFilters(rtRCDATA);
              LoadDlg.DefaultExt:=GetDefaultExtension(rtBITMAP);
              Handled:=True;
              if LoadDlg.Execute then
                DotNetAddFileToResource(LoadDlg.FileName, 'FILE', rdDNet.ResourceName, NodeData.ResourceFile);
            end;
          finally
            LoadDlg.Free;
          end;
        end;

        if Handled then
          LoadResources
        else
        begin
          if EditDotNetResource(TDotNetResourceElement(NodeData.Resource)) then
          begin
            ResX:=LoadResX(NodeData.ResourceFile);

            for i := 0 to ResX.Data.Count - 1 do
              if (ResX.Data[i].Name=NodeData.ResName) then
              begin
                ResX.Data[i].Comment:=TDotNetResourceElement(NodeData.Resource).Comment;
                ResX.Data[i].Value:=TDotNetResourceElement(NodeData.Resource).Text;
              end;

            ResX.OwnerDocument.SaveToFile(NodeData.ResourceFile);
            MarkProjectModified;
          end;
        end;
      end;
  end;
end;

function TFormWizardResEd.CreateResource(ParentNode : PVirtualNode;
                                         ResType    : TResType;
                                         GroupID    : String;
                                         FileName   : String): TResourceElement;
var
  NodeData : PNodeData;
  NewName  : String;
  DotNetType: String;
begin
  result:=nil;

  while ParentNode.Parent<>TV.RootNode do
    ParentNode:=ParentNode.Parent;

  NodeData:=TV.GetNodeData(ParentNode);

  NewName:=ExtractFileName(FileName);
  NewName:=UpperCase(AnsiReplaceText(NewName,ExtractFileExt(NewName),''));
  NewName:=AnsiReplaceStr(NewName,' ','_');

  if (ResNameGlob<>'') then
    NewName:=ResNameGlob;

  if (NodeData.IsDotNet) then
  begin
    if (ExtDotNetSupport) then
    begin
      case ResType of
        rtBITMAP : DotNetType:='BITMAP';
        rtCURSOR : DotNetType:='ICON';
        rtICON   : DotNetType:='ICON';
        rtJPEG   : DotNetType:='BITMAP';
        rtGIF    : DotNetType:='BITMAP';
        rtPNG    : DotNetType:='BITMAP';

        rtRCDATA : DotNetType:='FILE';
        rtDotNetCustom: DotNetType:='OTHER';
      else
        DotNetType:='';
      end;

      DotNetAddFileToResource(FileName, DotNetType, NewName, NodeData.ResourceFile);

      miRefresh.Click;
    end
    else
      MessageDlg('for extended DotNet support install ResXEd.dll into your bpl folder', mtInformation, [mbOk], 0);
  end
  else
  begin
    case ResType of
      rtBITMAP : begin
                   Result:=TBitmapResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   (Result as TBitmapResourceElement).LoadImage(FileName);
                 end;
      rtCURSOR : begin
                   Result:=TCursorGroupResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   (Result as TCursorGroupResourceElement).LoadImage(FileName);
                 end;
      rtICON   : begin
                   Result:=TIconGroupResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   (Result as TIconGroupResourceElement).LoadImage(FileName);
                 end;
      rtJPEG   : begin
                   Result:=TJPegResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   (Result as TJPegResourceElement).Data.LoadFromFile(FileName);
                 end;
      rtGIF    : begin
                   Result:=TGifResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   (Result as TGifResourceElement).Data.LoadFromFile(FileName);
                 end;
      rtPNG    : begin
                   Result:=TPngResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   (Result as TPngResourceElement).Data.LoadFromFile(FileName);
                 end;
      rtRCDATA : begin
                   if GroupID='' then
                     Result:=TRCDataResourceElement.CreateNew(NodeData.ResFile,0,NewName)
                   else
                     Result:=TRCDataResourceElement.CreateNew(NodeData.ResFile,0,NewName,GroupID);
                   (Result as TRCDataResourceElement).Data.LoadFromFile(FileName);
                 end;
      rtMESSAGETABLE :
                 begin
                   Result:=TMessageResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   EditStringResource(Result as TTextResourceElement);
                 end;
      rtSTRING : begin
                   Result:=TStringResourceElement.CreateNew(NodeData.ResFile,0,NewName);
                   EditStringResource(Result as TTextResourceElement);
                 end;
      rtVERSIONINFO: begin
                   Result:=TVersionInfoResourceElement.CreateNew(NodeData.ResFile,0, NewName);
                   EditVersionInfoResource(Result as TVersionInfoResourceElement);
                 end;
      rtMANIFEST: begin
                     Result:=TXPManifestResourceElement.CreateNew(NodeData.ResFile,0, NewName);
                   //  EditVersionInfoResource(Result as TVersionInfoResourceElement);
                  end;
    end;

    HandleResDetail(ParentNode,Result,NodeData.ResFile,NodeData.ResFile.IndexOfResource(Result), NodeData.ResourceFile, NodeData.IsDotNet);

    NodeData.ResFile.SaveToFile(NodeData.ResourceFile);
  end;
  MarkProjectModified;
end;

procedure TFormWizardResEd.CreateResourceClick(Sender: TObject);
var
  Node    : PVirtualNode;
  dlgOpen : TOpenDialog;
  rt      : TResType;
  i       : integer;
  Group   : String;
  NewName : String;
begin
  Node:=TV.GetFirstSelected;

  if Sender=miManifest then
    rt:=rtManifest
  else
  if Sender=miDotNetCustomData then
    rt:=rtDotNetCustom
  else
  if Sender=miBITMAP then
    rt:=rtBITMAP
  else
  if Sender=miICON then
    rt:=rtICON
  else
  if Sender=miCURSOR then
    rt:=rtCURSOR
  else
  if Sender=miGIF then
    rt:=rtGIF
  else
  if Sender=miJPEG then
    rt:=rtJPEG
  else
  if Sender=miVersionInfo then
    rt:=rtVERSIONINFO
  else
  if Sender=miPNG then
    rt:=rtPNG
  else
  if Sender=miStringTable then
    rt:=rtSTRING
  else
  if Sender=miMessageTable then
    rt:=rtMESSAGETABLE
  else
  if Sender=miUserData then
  begin
    rt:=rtRCDATA;
    Group:=UpperCase(InputBox('Group ID','Please enter a group identifier',''));
    if Group='' then
      Exit;
  end
  else
  begin
    rt:=rtRCDATA;
    Group:='';
  end;

  dlgOpen:=TOpenDialog.Create(nil);
  dlgOpen.Filter:=GetFilters(rt);
  dlgOpen.Options:=dlgOpen.Options + [ofAllowMultiSelect];

  if (rt = rtVERSIONINFO) then
  begin
    CreateResource(Node,rt,Group,'1');
  end
  else
  if (rt = rtMANIFEST) then
  begin
    CreateResource(Node,rt,Group,'1');
  end
  else
  if (rt = rtDotNetCustom) then
  begin
    CreateResource(Node,rt,Group,'');
  end
  else
  if not (rt in [rtSTRING, rtMESSAGETABLE]) then
  begin
    if dlgOpen.Execute(self.Handle) then
    begin
      for i:=0 to dlgOpen.Files.Count-1 do
        CreateResource(Node,rt,Group,dlgOpen.Files[i]);
      if (Node.ChildCount>0) then
        Tv.Expanded[Tv.GetFirstChild(Node)]:=true;
    end;
  end
  else
  begin
    if rt=rtSTRING then
    begin
      NewName:=InputBox('String Table Offset',
                        'Please enter the new string table offset',
                        '');
      if NewName='' then
        raise Exception.Create('No offset specified');

      NewName:=StringsIdToResId(NewName);
    end
    else
    begin
      NewName:=InputBox('Message Table',
                        'Please enter a name for the message table',
                        '');
      if NewName='' then
        raise Exception.Create('No name specified');
    end;

    CreateResource(Node,rt,'',NewName);

    if (Node.ChildCount>0) then
        Tv.Expanded[Tv.GetFirstChild(Node)]:=true;
  end;
end;
{$EndRegion}

{$Region 'Popup Menu actions'}
procedure TFormWizardResEd.miCreateResourceFileClick(Sender: TObject);
var
  ActiveProject : IOTAProject;

  Editor        : IOTASourceEditor;
  Reader        : IOTAEditReader;
  Writer        : IOTAEditWriter;
  sl            : TStrings;
  i             : Integer;
  FileName      : AnsiString;

  Buffer        : Array [0..1000] of AnsiChar;
  BufPos        : Integer;
  RegExp        : TRegExpr;
  insertPos     : Integer;
  perso         : String;
  IsDotNet      : Boolean;
begin
  ActiveProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  if ActiveProject= nil then
  begin
    MessageDlg('No active project!', mtError, [mbOK], 0);
    Exit;
  end;

  if (not CheckPersonality(ActiveProject,perso,IsDotNet)) or IsDotNet  then
    raise Exception.Create('Automatic resource file generation for '+perso+
                           ' not supported by current version of ResEd');

  FileName:=InputBox('Create New Resource File','Please enter the name for the new resource file:','');

  if FileName<>'' then
  begin
    if not AnsiEndsText('.res',FileName) then
      FileName:=FileName+'.res';

    if ActiveProject.GetModuleFileCount>0 then
      Editor := (ActiveProject.GetModuleFileEditor(0) as IOTASourceEditor);


    if ActiveProject.Personality='Delphi.Personality' then
    begin
      sl:=TStringList.Create;
      Reader:=Editor.CreateReader;
      RegExp:=TRegExpr.Create;
      RegExp.ModifierI:=True;
      RegExp.Expression:='(package|library|program) \w+;';
      insertPos:=0;
      try
        BufPos := 0;
        while Reader.GetText(BufPos, @Buffer, SizeOf(Buffer))>0 do
        begin
          sl.text:=sl.Text+Buffer;
          Inc(BufPos, SizeOf(Buffer));
        end;

        for i:=0 to sl.Count-1 do
        begin
          inc(insertPos,Length(sl[i])+2);

          if RegExp.Exec(sl[i]) then
          begin
            Writer:=Editor.CreateUndoableWriter;
            Writer.CopyTo(insertPos-2);
            Writer.Insert(PAnsiChar(#13#10'{$R '''+FileName+'''}'#13#10));
            break;
          end
        end;

        Editor.Show;
        Editor.MarkModified;
        Editor.Module.Save(false,true);
      finally
        RegExp.Free;
        sl.Free;
        Reader:=nil;
        Writer:=nil;
      end;
    end
    else
    if (ActiveProject.Personality='CPlusPlusBuilder.Personality') then
    begin
      ActiveProject.AddFile(FileName, False);
    end;

    LoadResources;
  end;
end;

procedure TFormWizardResEd.miRefreshClick(Sender: TObject);
begin
  LoadResources;
end;

procedure TFormWizardResEd.PopupEditorPopup(Sender: TObject);
var
  myNode   : PVirtualNode;
  NodeData : PNodeData;
  s        : String;
begin
  myNode:=TV.GetFirstSelected;

  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);
    miCreateResourceFile.Enabled:=not NodeData.IsDotNet;
    miDelete.Enabled:=(NodeData.NodeType=ntResource);
    if NodeData.IsDotNet and (not ExtDotNetSupport) then
      miDelete.Enabled:=False;
    miChange.Enabled:=NodeData.NodeType=ntResource;
    miRename.Enabled:=NodeData.NodeType=ntResource;
    miSaveToFile.Enabled:=NodeData.NodeType=ntResource;
    miNewResource.Enabled:=(not (NodeData.NodeType=ntResource));
    if NodeData.IsDotNet and (not ExtDotNetSupport) then
      miNewResource.Enabled:=False;
    miMessageTable.Enabled:=(not (NodeData.NodeType=ntResource)) and (not NodeData.IsDotNet);
    miStringTable.Enabled:=(not (NodeData.NodeType=ntResource)) and (not NodeData.IsDotNet);
    miVersionInfo.Enabled:=(not (NodeData.NodeType=ntResource)) and (not NodeData.IsDotNet);
    miDotNetCustomData.Enabled:=(not (NodeData.NodeType=ntResource)) and (NodeData.IsDotNet) and (ExtDotNetSupport);
    miAddToSrc.Enabled:=NodeData.NodeType=ntResource;
    miResourceLanguage.Enabled:=(NodeData.NodeType=ntResource) and (not NodeData.IsDotNet);

    miSetNeutral.Caption:=StrNoLanguage;
    s:='';
    if miResourceLanguage.Enabled then
    begin
      if NodeData.Resource.ResourceLanguage=0 then
        s := StrNoLanguage
      else
        s:=Languages.Name[Languages.IndexOf(NodeData.Resource.ResourceLanguage)]+
           '['+Languages.ID[Languages.IndexOf(NodeData.Resource.ResourceLanguage)]+']'
    end;

    miCurrentResLang.Caption:=s;
  end
  else
  begin
    miDelete.Enabled:=false;
    miChange.Enabled:=false;
    miRename.Enabled:=false;
    miResourceLanguage.Enabled:=False;
    miSaveToFile.Enabled:=false;
    miNewResource.Enabled:=false;
    miAddToSrc.Enabled:=false;
  end;
end;

procedure TFormWizardResEd.miSaveToFileClick(Sender: TObject);
var
  myNode   : PVirtualNode;
  NodeData : PNodeData;
  pic      : TPicture;
  SaveDlg  : TSaveDialog;
  resType  : TResType;
begin
  myNode:=TV.GetFirstSelected;
  Pic:=Nil;

  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);
    if NodeData.NodeType=ntResource then
    begin
      resType:=GetResTypeFromClassType(NodeData.Resource.ClassType);

      SaveDlg:=TSaveDialog.Create(nil);
      SaveDlg.Filter:=GetFilters(resType);
      SaveDlg.DefaultExt:=GetDefaultExtension(resType);
      try
        if SaveDlg.Execute then
        begin
          case resType of
            rtBitmap  : begin
                          Pic:=TPicture.Create;
                          (NodeData.Resource as TBitmapResourceElement).GetImage(Pic);
                        end;
            rtCURSOR  : begin
                          Pic:=TPicture.Create;
                          (NodeData.Resource as TCursorGroupResourceElement).GetImage(Pic);
                        end;
            rtICON    : begin
                          Pic:=TPicture.Create;
                          (NodeData.Resource as TIconGroupResourceElement).GetImage(Pic);
                        end;
            rtJPEG    : begin
                          Pic:=TPicture.Create;
                          (NodeData.Resource as TJPegResourceElement).GetImage(Pic);
                        end;
            rtPNG     : begin
                          Pic:=TPicture.Create;
                          (NodeData.Resource as TPngResourceElement).GetImage(Pic);
                        end;
            rtGIF     : begin
                          Pic:=TPicture.Create;
                          (NodeData.Resource as TGifResourceElement).GetImage(Pic);
                        end;
            rtRCDATA  : begin
                          NodeData.Resource.Data.SaveToFile(SaveDlg.FileName);
                        end;
          end;

          if Assigned(pic) then
          begin
            try
              Pic.Graphic.SaveToFile(SaveDlg.FileName);
            finally
              Pic.Free;
            end;
          end;
        end;
      finally
        SaveDlg.Free;
      end;
    end;
  end;
end;

procedure TFormWizardResEd.miAddToSrcClick(Sender: TObject);
var
  myNode       : PVirtualNode;
  NodeData     : PNodeData;
begin
  myNode:=TV.GetFirstSelected;
  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);
    if NodeData.NodeType=ntResource then
    begin
      AddStringToSource(NodeData.ResName);
    end;
  end;
end;

procedure TFormWizardResEd.miRenameClick(Sender: TObject);
var
  myNode    : PVirtualNode;
  NodeData  : PNodeData;
begin
  myNode:=TV.GetFirstSelected;

  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);
    if NodeData.NodeType=ntResource then
    begin
      //ToDo: If the are columns in the Tree, you have to change the column-number below
      Tv.EditNode(myNode,-1);
    end;
  end;
end;

procedure TFormWizardResEd.miDeleteClick(Sender: TObject);
var
  myNode     : PVirtualNode;
  NodeData   : PNodeData;
  idx        : Integer;
begin
  if TV.IsEditing then
  begin
    ActiveControl.Perform(WM_KEYDOWN, VK_DELETE, 0);
    Exit;
  end;
    
  myNode:=TV.GetFirstSelected;

  if myNode<>nil then
  begin
    NodeData:=TV.GetNodeData(myNode);

    if NodeData.NodeType=ntResource then
    begin
      if NodeData.IsDotNet then
      begin
        if ExtDotNetSupport then
        begin
          DotNetRemoveFromResource(NodeData.ResName, NodeData.ResourceFile);
          TV.DeleteNode(myNode);
        end
      end
      else
      begin
        idx:=NodeData.ResFile.IndexOfResource(NodeData.Resource);
        NodeData.ResFile.DeleteResource(idx);

        if (not NodeData.IsDotNet) then
          NodeData.ResFile.SaveToFile(NodeData.ResourceFile);
        MarkProjectModified;

        if TV.GetNext(myNode) <> nil then
        begin
          TV.FocusedNode:=TV.GetNext(MyNode);
          TV.Selected[TV.GetNext(MyNode)]:=True;
        end;

        TV.DeleteNode(myNode);
      end;
    end;
  end;
end;
{$EndRegion}

end.
