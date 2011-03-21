unit uPS_Classes;

interface

uses
  uPSCompiler,
  uPSRuntime,
  Classes,
  SysUtils,
  uPSUtils,
  uPS_Helpers;

procedure PS_Register_Classes_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_Classes_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

procedure TComponent_Components_R(Self: TComponent; var T: TComponent; I : Integer); begin T := Self.Components[I]; end;
procedure TComponent_ComponentCount_R(Self: TComponent; var T: Integer); begin T := Self.ComponentCount; end;
procedure TComponent_ComponentIndex_R(Self: TComponent; var T: Integer); begin T := Self.ComponentIndex; end;
procedure TComponent_ComponentIndex_W(Self: TComponent; const T: Integer); begin Self.ComponentIndex := T; end;
procedure TComponent_ComponentState_R(Self: TComponent; var T: TComponentState); begin T := Self.ComponentState; end;
procedure TComponent_ComponentStyle_R(Self: TComponent; var T: TComponentStyle); begin T := Self.ComponentStyle; end;
procedure TComponent_DesignInfo_R(Self: TComponent; var T: LongInt); begin T := Self.DesignInfo; end;
procedure TComponent_DesignInfo_W(Self: TComponent; const T: LongInt); begin Self.DesignInfo := T; end;
procedure TComponent_Owner_R(Self: TComponent; var T: TComponent); begin T := Self.Owner; end;
procedure TComponent_Name_R(Self: TComponent; var T: String); begin T := Self.Name; end;
procedure TComponent_Name_W(Self: TComponent; const T: String); begin Self.Name := T; end;
procedure TComponent_Tag_R(Self: TComponent; var T: LongInt); begin T := Self.Tag; end;
procedure TComponent_Tag_W(Self: TComponent; const T: LongInt); begin Self.Tag := T; end;

procedure TStream_Position_R(Self: TStream; var T: Int64); begin T := Self.Position; end;
procedure TStream_Position_W(Self: TStream; const T: Int64); begin Self.Position := T; end;
procedure TStream_Size_R(Self: TStream; var T: Int64); begin T := Self.Size; end;
procedure TStream_Size_W(Self: TStream; const T: Int64); begin Self.Size := T; end;

procedure THandleStream_Handle_R(Self: THandleStream; var T: Cardinal); begin T := Self.Handle; end;

procedure TFileStream_Filename_R(Self: TFileStream; var T: String); begin T := Self.FileName; end;

procedure TStrings_Capacity_R(Self: TStrings; var T: Integer); begin T := Self.Capacity; end;
procedure TStrings_Capacity_W(Self: TStrings; const T: Integer); begin Self.Capacity := T; end;
procedure TStrings_CommaText_R(Self: TStrings; var T: String); begin T := Self.CommaText; end;
procedure TStrings_CommaText_W(Self: TStrings; const T: String); begin Self.CommaText := T; end;
procedure TStrings_Count_R(Self: TStrings; var T: Integer); begin T := Self.Count; end;
procedure TStrings_Delimiter_R(Self: TStrings; var T: Char); begin T := Self.Delimiter; end;
procedure TStrings_Delimiter_W(Self: TStrings; const T: Char); begin Self.Delimiter := T; end;
procedure TStrings_DelimitedText_R(Self: TStrings; var T: String); begin T := Self.DelimitedText; end;
procedure TStrings_DelimitedText_W(Self: TStrings; const T: String); begin Self.DelimitedText := T; end;
procedure TStrings_LineBreak_R(Self: TStrings; var T: String); begin T := Self.LineBreak; end;
procedure TStrings_LineBreak_W(Self: TStrings; const T: String); begin Self.LineBreak := T; end;
procedure TStrings_Names_R(Self: TStrings; var T: String; i : Integer); begin T := Self.Names[i]; end;
procedure TStrings_Objects_R(Self: TStrings; var T: TObject; i : Integer); begin T := Self.Objects[i]; end;
procedure TStrings_Objects_W(Self: TStrings; const T: TObject; i : Integer); begin Self.Objects[i] := T; end;
procedure TStrings_QuoteChar_R(Self: TStrings; var T: Char); begin T := Self.QuoteChar; end;
procedure TStrings_QuoteChar_W(Self: TStrings; const T: Char); begin Self.QuoteChar := T; end;
procedure TStrings_Values_R(Self: TStrings; var T: String; i : String); begin T := Self.Values[i]; end;
procedure TStrings_Values_W(Self: TStrings; const T: String; i : String); begin Self.Values[i] := T; end;
procedure TStrings_ValueFromIndex_R(Self: TStrings; var T: String; i : Integer); begin T := Self.ValueFromIndex[i]; end;
procedure TStrings_ValueFromIndex_W(Self: TStrings; const T: String; i : Integer); begin Self.ValueFromIndex[i] := T; end;
procedure TStrings_NameValueSeparator_R(Self: TStrings; var T: Char); begin T := Self.NameValueSeparator; end;
procedure TStrings_NameValueSeparator_W(Self: TStrings; const T: Char); begin Self.NameValueSeparator := T; end;
procedure TStrings_StrictDelimiter_R(Self: TStrings; var T: Boolean); begin T := Self.StrictDelimiter; end;
procedure TStrings_StrictDelimiter_W(Self: TStrings; const T: Boolean); begin Self.StrictDelimiter := T; end;
procedure TStrings_Strings_R(Self: TStrings; var T: String; i : Integer); begin T := Self.Strings[i]; end;
procedure TStrings_Strings_W(Self: TStrings; const T: String; i : Integer); begin Self.Strings[i] := T; end;
procedure TStrings_Text_R(Self: TStrings; var T: String); begin T := Self.Text; end;
procedure TStrings_Text_W(Self: TStrings; const T: String); begin Self.Text := T; end;

procedure TStringList_Duplicates_R(Self: TStringList; var T: TDuplicates); begin T := Self.Duplicates; end;
procedure TStringList_Duplicates_W(Self: TStringList; const T: TDuplicates); begin Self.Duplicates := T; end;
procedure TStringList_Sorted_R(Self: TStringList; var T: Boolean); begin T := Self.Sorted; end;
procedure TStringList_Sorted_W(Self: TStringList; const T: Boolean); begin Self.Sorted := T; end;
procedure TStringList_CaseSensitive_R(Self: TStringList; var T: Boolean); begin T := Self.CaseSensitive; end;
procedure TStringList_CaseSensitive_W(Self: TStringList; const T: Boolean); begin Self.CaseSensitive := T; end;
procedure TStringList_OwnsObjects_R(Self: TStringList; var T: Boolean); begin T := Self.OwnsObjects; end;
procedure TStringList_OwnsObjects_W(Self: TStringList; const T: Boolean); begin Self.OwnsObjects := T; end;
procedure TStringList_OnChange_R(Self: TStringList; var T: TNotifyEvent); begin T := Self.OnChange; end;
procedure TStringList_OnChange_W(Self: TStringList; const T: TNotifyEvent); begin Self.OnChange := T; end;
procedure TStringList_OnChanging_R(Self: TStringList; var T: TNotifyEvent); begin T := Self.OnChanging; end;
procedure TStringList_OnChanging_W(Self: TStringList; const T: TNotifyEvent); begin Self.OnChanging := T; end;



procedure PS_Register_Classes_C(ACompiler : TPSPascalCompiler);
var
  pscTPersistent,
  pscTComponent,
  pscTStream,
  pscTHandleStream,
  pscTFileStream,
  pscTCustomMemoryStream,
  pscTMemoryStream,
  pscTStrings,
  pscTStringList : TPSCompileTimeClass;
begin
  ACompiler.AddConstantN('fmCreate', 'Integer').Value.tu16 := fmCreate;

  ACompiler.AddTypeS('TNotifyEvent', 'procedure(Sender : TObject)');

  pscTPersistent := ACompiler.AddClass(ACompiler.FindClass('TObject'), TPersistent);
  with pscTPersistent do
  begin
    RegisterMethod('procedure Assign(Source: TPersistent); virtual;');
  end;


  ACompiler.AddTypeS('TComponentStateEnum', '(csLoading, csReading, csWriting, csDestroying,'+
                                          'csDesigning, csAncestor, csUpdating, csFixups,'+
                                          'csFreeNotification, csInline, csDesignInstance)');
  ACompiler.AddTypeS('TComponentState', 'set of TComponentStateEnum');
  ACompiler.AddTypeS('TComponentStyleEnum', '(csInheritable, csCheckPropAvail, csSubComponent,csTransient)');
  ACompiler.AddTypeS('TComponentStyle', 'set of TComponentStyleEnum');
  ACompiler.AddType('TComponentName', btString);

  pscTComponent := ACompiler.AddClass(pscTPersistent, TComponent);
  with pscTComponent do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent); virtual;');
    RegisterMethod('procedure DestroyComponents;');
    RegisterMethod('procedure Destroying;');
    RegisterMethod('function FindComponent(const AName: string): TComponent;');
    RegisterMethod('procedure FreeNotification(AComponent: TComponent);');
    RegisterMethod('procedure RemoveFreeNotification(AComponent: TComponent);');
    RegisterMethod('procedure FreeOnRelease;');
    RegisterMethod('procedure InsertComponent(AComponent: TComponent);');
    RegisterMethod('procedure RemoveComponent(AComponent: TComponent);');
    RegisterProperty('Components', 'TComponent Integer', iptR);
    RegisterProperty('ComponentCount', 'Integer', iptR);
    RegisterProperty('ComponentIndex', 'Integer', iptRW);
    RegisterProperty('ComponentState', 'TComponentState', iptR);
    RegisterProperty('ComponentStyle', 'TComponentStyle', iptR);
    RegisterProperty('DesginInfo', 'LongInt', iptRW);
    RegisterProperty('Owner', 'TComponent', iptR);
    RegisterProperty('Name', 'TComponentName', iptRW);
    RegisterProperty('Tag', 'Integer', iptRW);
  end;

  ACompiler.AddConstantN('soFromBeginning', 'Word').Value.tu16 := soFromBeginning;
  ACompiler.AddConstantN('soFromCurrent', 'Word').Value.tu16 := soFromCurrent;
  ACompiler.AddConstantN('soFromEnd', 'Word').Value.tu16 := soFromEnd;

  RegisterEnum(ACompiler, TypeInfo(TSeekOrigin));

  pscTStream := ACompiler.AddClass(ACompiler.FindClass('TObject'), TStream);
  with pscTStream do
  begin
    IsAbstract := true;
    RegisterMethod('function Read(Buffer : AnsiString; Count: Longint): Longint;');
    RegisterMethod('function Write(Buffer : AnsiString; Count: Longint): Longint;');
    RegisterMethod('function Seek(Offset: Longint; Origin: Word): Longint;');
    RegisterMethod('procedure ReadBuffer(Buffer : AnsiString; Count: Longint);');
    RegisterMethod('procedure WriteBuffer(Buffer : AnsiString; Count: Longint);');
    RegisterMethod('function CopyFrom(Source: TStream; Count: Int64): Int64;');
    RegisterProperty('Position', 'Int64', iptRW);
    RegisterProperty('Size', 'Int64', iptRW);
  end;

  pscTCustomMemoryStream := ACompiler.AddClass(pscTStream, TCustomMemoryStream);
  with pscTCustomMemoryStream do
  begin
    IsAbstract := true;
    RegisterMethod('function Read(Buffer : AnsiString; Count: Longint): Longint; override;');
    RegisterMethod('function Seek(Offset: Longint; Origin: Word): Longint; override;');
    RegisterMethod('procedure SaveToStream(Stream: TStream);');
    RegisterMethod('procedure SaveToFile(const FileName: string);');
  end;

  pscTMemoryStream := ACompiler.AddClass(pscTCustomMemoryStream, TMemoryStream);
  with pscTMemoryStream do
  begin
    RegisterMethod('procedure Clear;');
    RegisterMethod('procedure LoadFromStream(Stream: TStream);');
    RegisterMethod('procedure LoadFromFile(const FileName: string);');
    RegisterMethod('procedure SetSize(NewSize: Longint); override;');
  end;

  pscTHandleStream := ACompiler.AddClass(pscTStream, THandleStream);
  with pscTHandleStream do
  begin
    RegisterMethod('constructor Create(AHandle: Cardinal);');
    RegisterProperty('Handle', 'Cardinal', iptR);
  end;

  pscTFileStream := ACompiler.AddClass(pscTHandleStream, TFileStream);
  with pscTFileStream do
  begin
    RegisterMethod('constructor Create(const AFileName: string; Mode: Word);');
    RegisterProperty('Filename', 'String', iptR);
  end;

  pscTStrings := ACompiler.AddClass(pscTPersistent, TStrings);
  with pscTStrings do
  begin
    IsAbstract := true;
    RegisterMethod('function Add(const S: string): Integer; virtual;');
    RegisterMethod('function AddObject(const S: string; AObject: TObject): Integer; virtual;');
    RegisterMethod('procedure Append(const S: string);');
    RegisterMethod('procedure AddStrings(Strings: TStrings); virtual;');
    RegisterMethod('procedure Assign(Source: TPersistent); override;');
    RegisterMethod('procedure BeginUpdate;');
    RegisterMethod('procedure Clear; virtual; abstract;');
    RegisterMethod('procedure Delete(Index: Integer); virtual; abstract;');
    RegisterMethod('procedure EndUpdate;');
    RegisterMethod('procedure Exchange(Index1, Index2: Integer); virtual;');
    RegisterMethod('function IndexOf(const S: string): Integer; virtual;');
    RegisterMethod('function IndexOfName(const Name: string): Integer; virtual;');
    RegisterMethod('function IndexOfObject(AObject: TObject): Integer; virtual;');
    RegisterMethod('procedure Insert(Index: Integer; const S: string); virtual; abstract;');
    RegisterMethod('procedure InsertObject(Index: Integer; const S: string; AObject: TObject); virtual;');
    RegisterMethod('procedure LoadFromFile(const FileName: string); virtual;');
    RegisterMethod('procedure LoadFromStream(Stream: TStream); virtual;');
    RegisterMethod('procedure Move(CurIndex, NewIndex: Integer); virtual;');
    RegisterMethod('procedure SaveToFile(const FileName: string); virtual;');
    RegisterMethod('procedure SaveToStream(Stream: TStream); virtual;');
    RegisterProperty('Capacity', 'Integer', iptRW);
    RegisterProperty('CommaText', 'String', iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Delimiter', 'Char', iptRW);
    RegisterProperty('DelimitedText', 'DelimitedText', iptRW);
    RegisterProperty('LineBreak', 'String', iptRW);
    RegisterProperty('Names', 'String Integer', iptR);
    RegisterProperty('Objects', 'TObject Integer', iptRW);
    RegisterProperty('QuoteChar', 'Char', iptRW);
    RegisterProperty('Values', 'String String', iptRW);
    RegisterProperty('ValueFromIndex', 'String Integer', iptRW);
    RegisterProperty('NameValueSeparator', 'Char', iptRW);
    RegisterProperty('StrictDelimiter', 'Boolean', iptRW);
    RegisterProperty('Strings', 'String Integer', iptRW);
    SetDefaultPropery('Strings');
    RegisterProperty('Text', 'String', iptRW);
  end;

  RegisterEnum(ACompiler, TypeInfo(TDuplicates));

  pscTStringList := ACompiler.AddClass(pscTStrings, TStringList);
  with pscTStringList do
  begin
    RegisterMethod('function Find(const S: string; var Index: Integer): Boolean; virtual;');
    RegisterMethod('procedure Sort; virtual;');
    RegisterProperty('Duplicates', 'TDuplicates', iptRW);
    RegisterProperty('Sorted', 'Boolean', iptRW);
    RegisterProperty('CaseSensitive', 'Boolean', iptRW);
    RegisterProperty('OwnsObjects', 'Boolean', iptRW);
    RegisterProperty('OnChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnChanging', 'TNotifyEvent', iptRW);
  end;
end;

procedure PS_Register_Classes_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  with ARCi.Add(TPersistent) do
  begin
    RegisterVirtualMethod(@TPersistent.Assign, 'Assign');
  end;

  with ARCi.Add(TComponent) do
  begin
    RegisterVirtualConstructor(@TComponent.Create, 'Create');
    RegisterMethod(@TComponent.DestroyComponents, 'DestroyComponents');
    RegisterMethod(@TComponent.Destroying, 'Destroying');
    RegisterMethod(@TComponent.FindComponent, 'FindComponent');
    RegisterMethod(@TComponent.FreeNotification, 'FreeNotification');
    RegisterMethod(@TComponent.RemoveFreeNotification, 'RemoveFreeNotification');
    RegisterMethod(@TComponent.FreeOnRelease, 'FreeOnRelease');
    RegisterMethod(@TComponent.InsertComponent, 'InsertComponent');
    RegisterMethod(@TComponent.RemoveComponent, 'RemoveComponent');
    RegisterPropertyHelper(@TComponent_Components_R, nil, 'Components');
    RegisterPropertyHelper(@TComponent_ComponentCount_R, nil, 'ComponentCount');
    RegisterPropertyHelper(@TComponent_ComponentIndex_R, @TComponent_ComponentIndex_W, 'ComponentIndex');
    RegisterPropertyHelper(@TComponent_ComponentState_R, nil, 'ComponentState');
    RegisterPropertyHelper(@TComponent_ComponentStyle_R, nil, 'ComponentStyle');
    RegisterPropertyHelper(@TComponent_DesignInfo_R, @TComponent_DesignInfo_W, 'DesignInfo');
    RegisterPropertyHelper(@TComponent_Owner_R, nil, 'Owner');
    RegisterPropertyHelper(@TComponent_Name_R, @TComponent_Name_W, 'Name');
    RegisterPropertyHelper(@TComponent_Tag_R, @TComponent_Tag_W, 'Tag');
  end;

  with arci.Add(TStream) do
  begin
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.Read, 'Read');
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.Write, 'Write');
    RegisterVirtualAbstractMethod(TMemoryStream, @TMemoryStream.Seek, 'Seek');
    RegisterMethod(@TStream.ReadBuffer, 'ReadBuffer');
    RegisterMethod(@TStream.WriteBuffer, 'WriteBuffer');
    RegisterMethod(@TStream.CopyFrom, 'CopyFrom');
    RegisterPropertyHelper(@TStream_Position_R, @TStream_Position_W, 'Position');
    RegisterPropertyHelper(@TStream_Size_R, @TStream_Size_W, 'Size');
  end;

  with ARCi.Add(TCustomMemoryStream) do
  begin
    RegisterVirtualMethod(@TCustomMemoryStream.Read, 'Read');
    RegisterVirtualMethod(@TCustomMemoryStream.Seek, 'Seek');
    RegisterMethod(@TCustomMemoryStream.SaveToStream, 'SaveToStream');
    RegisterMethod(@TCustomMemoryStream.SaveToFile, 'SaveToFile');
  end;

  with ARCi.Add(TMemoryStream) do
  begin
    RegisterMethod(@TMemoryStream.Clear, 'Clear');
    RegisterMethod(@TMemoryStream.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TMemoryStream.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TMemoryStream.SetSize, 'SetSize');
  end;

  with ARCi.Add(THandleStream) do
  begin
    RegisterConstructor(@THandleStream.Create, 'Create');
    RegisterPropertyHelper(@THandleStream_Handle_R, nil, 'Handle');
  end;

  with ARCi.Add(TFileStream) do
  begin
    RegisterConstructor(@TFileStream.Create, 'Create');
    RegisterPropertyHelper(@TFileStream_Filename_R, nil, 'Filename');
  end;

  with ARCi.Add(TStrings) do
  begin
    RegisterVirtualMethod(@TStrings.Add, 'Add');
    RegisterVirtualMethod(@TStrings.AddObject, 'AddObject');
    RegisterMethod(@TStrings.Append, 'Append');
    RegisterVirtualMethod(@TStrings.AddStrings, 'AddStrings');
    RegisterVirtualMethod(@TStrings.Assign, 'Assign');
    RegisterMethod(@TStrings.BeginUpdate, 'BeginUpdate');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Clear, 'Clear');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Delete, 'Delete');
    RegisterMethod(@TStrings.EndUpdate, 'EndUpdate');
    RegisterVirtualMethod(@TStrings.Exchange, 'Exchange');
    RegisterVirtualMethod(@TStrings.IndexOf, 'IndexOf');
    RegisterVirtualMethod(@TStrings.IndexOfName, 'IndexOfName');
    RegisterVirtualMethod(@TStrings.IndexOfObject, 'IndexOfObject');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Insert, 'Insert');
    RegisterVirtualMethod(@TStrings.InsertObject, 'InsertObject');
    RegisterVirtualMethod(@TStrings.LoadFromFile, 'LoadFromFile');
    RegisterVirtualMethod(@TStrings.LoadFromStream, 'LoadFromStream');
    RegisterVirtualMethod(@TStrings.Move, 'Move');
    RegisterVirtualMethod(@TStrings.SaveToFile, 'SaveToFile');
    RegisterVirtualMethod(@TStrings.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TStrings_Capacity_R, @TStrings_Capacity_W, 'Capacity');
    RegisterPropertyHelper(@TStrings_CommaText_R, @TStrings_CommaText_W, 'CommaText');
    RegisterPropertyHelper(@TStrings_Count_R, nil, 'Count');
    RegisterPropertyHelper(@TStrings_Delimiter_R, @TStrings_Delimiter_W, 'Delimiter');
    RegisterPropertyHelper(@TStrings_DelimitedText_R, @TStrings_DelimitedText_W, 'DelimitedText');
    RegisterPropertyHelper(@TStrings_LineBreak_R, @TStrings_LineBreak_W, 'LineBreak');
    RegisterPropertyHelper(@TStrings_Names_R, nil, 'Names');
    RegisterPropertyHelper(@TStrings_Objects_R, @TStrings_Objects_W, 'Objects');
    RegisterPropertyHelper(@TStrings_QuoteChar_R, @TStrings_QuoteChar_W, 'QuoteChar');
    RegisterPropertyHelper(@TStrings_Values_R, @TStrings_Values_W, 'Values');
    RegisterPropertyHelper(@TStrings_ValueFromIndex_R, @TStrings_ValueFromIndex_W, 'ValueFromIndex');
    RegisterPropertyHelper(@TStrings_NameValueSeparator_R, @TStrings_NameValueSeparator_W, 'NameValueSeparator');
    RegisterPropertyHelper(@TStrings_StrictDelimiter_R, @TStrings_StrictDelimiter_W, 'StrictDelimiter');
    RegisterPropertyHelper(@TStrings_Strings_R, @TStrings_Strings_W, 'Strings');
    RegisterPropertyHelper(@TStrings_Text_R, @TStrings_Text_W, 'Text');
  end;

  with ARCi.Add(TStringList) do
  begin
    RegisterVirtualMethod(@TStringList.Find, 'Find');
    RegisterVirtualMethod(@TStringList.Sort, 'Sort');
    RegisterPropertyHelper(@TStringList_Duplicates_R, @TStringList_Duplicates_W, 'Duplicates');
    RegisterPropertyHelper(@TStringList_Sorted_R, @TStringList_Sorted_W, 'Sorted');
    RegisterPropertyHelper(@TStringList_CaseSensitive_R, @TStringList_CaseSensitive_W, 'CaseSensitive');
    RegisterPropertyHelper(@TStringList_OwnsObjects_R, @TStringList_OwnsObjects_W, 'OwnsObjects');
    RegisterEventPropertyHelper(@TStringList_OnChange_R, @TStringList_OnChange_W, 'OnChange');
    RegisterEventPropertyHelper(@TStringList_OnChanging_R, @TStringList_OnChanging_W, 'OnChanging');
  end;
end;

end.
