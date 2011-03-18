unit uPS_Classes;

interface

uses
  uPSCompiler,
  uPSRuntime,
  Classes,
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
procedure TStream_Size_R(Self: TStream; var T: Int64); begin T := Self.Size; end;
procedure TStream_Size_W(Self: TStream; const T: Int64); begin Self.Size := T; end;

procedure THandleStream_Handle_R(Self: THandleStream; var T: Cardinal); begin T := Self.Handle; end;


procedure PS_Register_Classes_C(ACompiler : TPSPascalCompiler);
var
  pscTPersistent,
  pscTComponent,
  pscTStream,
  pscTHandleStream,
  pscTFileStream : TPSCompileTimeClass;
begin
  ACompiler.AddConstantN('fmCreate', 'Integer').Value.tu16 := fmCreate;

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

  RegisterEnum(ACompiler, TypeInfo(TSeekOrigin));

  pscTStream := ACompiler.AddClass(ACompiler.FindClass('TObject'), TStream);
  with pscTStream do
  begin
    IsAbstract := true;
    RegisterMethod('function Read(var Buffer : String; Count: Longint): Longint;');
    RegisterMethod('function Write(Buffer : String; Count: Longint): Longint;');
    RegisterMethod('function Seek(Offset: Int64; Origin: TSeekOrigin): In64;');
    RegisterMethod('procedure ReadBuffer(Buffer : String; Count: Longint);');
    RegisterMethod('procedure WriteBuffer(Buffer : String; Count: Longint);');
    RegisterMethod('function CopyFrom(Source: TStream; Count: Int64): Int64;');
    RegisterProperty('Position', 'Int64', iptR);
    RegisterProperty('Size', 'Int64', iptRW);
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
    RegisterVirtualAbstractMethod(THandleStream, @THandleStream.Read, 'Read');
    RegisterVirtualAbstractMethod(THandleStream, @THandleStream.Write, 'Write');
    RegisterVirtualAbstractMethod(THandleStream, @THandleStream.Seek, 'Seek');
    RegisterMethod(@TStream.ReadBuffer, 'ReadBuffer');
    RegisterMethod(@TStream.WriteBuffer, 'WriteBuffer');
    RegisterMethod(@TStream.CopyFrom, 'CopyFrom');
    RegisterPropertyHelper(@TStream_Position_R, nil, 'Position');
    RegisterPropertyHelper(@TStream_Size_R, @TStream_Size_W, 'Size');
  end;

  with ARCi.Add(THandleStream) do
  begin
    RegisterConstructor(@THandleStream.Create, 'Create');
    RegisterPropertyHelper(@THandleStream_Handle_R, nil, 'Handle');
  end;

  with ARCi.Add(TFileStream) do
  begin
    RegisterConstructor(@TFileStream.Create, 'Create');
  end;

end;

end.
