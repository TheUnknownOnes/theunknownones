unit uFormCode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, VirtualTrees, RegExpr;

type
  TformCode = class(TForm)
    GroupBox1: TGroupBox;
    GridPanel1: TGridPanel;
    Label1: TLabel;
    edProcName: TEdit;
    Label2: TLabel;
    cbSingleResult: TCheckBox;
    GroupBox2: TGroupBox;
    edHead: TEdit;
    memBody: TMemo;
    procedure edProcNameChange(Sender: TObject);
    procedure cbSingleResultClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure memBodyEnter(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    function GenConstExpressionDef(AParentNode : PVirtualNode; var AIndex : Integer; out AVarName : String; ACode : TStrings) : Integer;
    procedure GenCode;
  public
    function RE(AInputString : String; AMatchList : TStrings; AExpression : Pointer = nil) : Integer;
    function RE3(AInputString: String; ADefaultResult: String = ''; AExpression: Pointer = nil): String;
  end;

implementation

uses uFormMain, uPattExCommon;

{$R *.dfm}

{ TformCode }

procedure TformCode.cbSingleResultClick(Sender: TObject);
begin
  GenCode;
end;

procedure TformCode.edProcNameChange(Sender: TObject);
begin
  GenCode;
end;

procedure TformCode.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TformCode.FormShow(Sender: TObject);
begin
  GenCode;
end;

procedure TformCode.GenCode;
var
  sl : TStringList;
  code : TStringlist;
  idx : Integer;
  varname : String;
begin
  code := TStringList.Create;

  edHead.Text := 'function ' + edProcName.Text + '(AInputString : String; ';

  if cbSingleResult.Checked then
    edHead.Text := edHead.Text + 'ADefault : String = ''''; AExpression : Pointer = nil) : String;'
  else
    edHead.Text := edHead.Text + 'AMatchList : TStrings; AExpression : Pointer = nil) : Integer;';

  code.Add('type');
  code.Add('  _PExpressions = ^_TExpressions;');
  code.Add('  _TExpression = record E, RS, CS : String; MG, MI, MM, MR, MS, MX, FMO : Boolean; Children : _PExpressions; ChildCount : Integer; end;');
  code.Add('  _PExpression = ^_TExpression;');
  code.Add('  _TExpressions = array[0..0] of _TExpression;');

  code.Add('const');
  sl := TStringList.Create;
  try
    idx := 0;
    GenConstExpressionDef(formMain.TVExpressions.RootNode, idx, varname, sl);
    code.AddStrings(sl);
  finally
    sl.Free;
  end;

  code.Add('var');
  code.Add('  _Expr : _PExpression absolute AExpression;');
  code.Add('  _ExprObj : _TExpression;');
  code.Add('  _RegEx : TRegExpr;');
  code.Add('  _Subst : String;');
  code.Add('  _idx : Integer;');

  code.Add('begin');
  if cbSingleResult.Checked then
  code.Add('  Result := ADefault;')
  else
  code.Add('  Result := 0;');

  code.Add('  if not Assigned(_Expr) then');
  code.Add('  begin');
  code.Add('    for _ExprObj in _0 do');
  code.Add('      Inc(Result, ' + edProcName.Text + '(AInputString, AMatchList, @_ExprObj));');
  code.Add('  end');
  code.Add('  else');
  code.Add('  begin');
  code.Add('    _RegEx := TRegExpr.Create;');
  code.Add('    try');
  code.Add('      _RegEx.Expression := _Expr^.E;');
  code.Add('      _RegEx.ModifierG := _Expr^.MG; _RegEx.ModifierI := _Expr^.MI; _RegEx.ModifierM := _Expr^.MM;');
  code.Add('      _RegEx.ModifierR := _Expr^.MR; _RegEx.ModifierS := _Expr^.MS; _RegEx.ModifierX := _Expr^.MX;');
  code.Add('');
  code.Add('      if _RegEx.Exec(AInputString) then');
  code.Add('      begin');
  code.Add('        repeat');
  code.Add('          if _Expr^.RS <> '''' then');
  code.Add('          begin');
  if cbSingleResult.Checked then
  code.Add('            Result := Result + _RegEx.Substitute(_Expr^.RS);')
  else
  begin
  code.Add('            AMatchList.Add(_RegEx.Substitute(_Expr^.RS));');
  code.Add('            Inc(Result);');
  end;
  code.Add('          end;');
  code.Add('');
  code.Add('          if _Expr^.CS <> '''' then');
  code.Add('          begin');
  code.Add('            _Subst := _RegEx.Substitute(_Expr^.CS);');
  code.Add('            for _idx := 0 to _Expr^.ChildCount - 1 do');
  if cbSingleResult.Checked then
  code.Add('              Result := Result + ' + edProcName.Text + '(_Subst, ADefaultResult, @_Expr^.Children[_idx]);')
  else
  code.Add('              Inc(Result, ' + edProcName.Text + '(_Subst, AMatchList, @_Expr^.Children[_idx]));');
  code.Add('          end;');
  code.Add('          if _Expr^.FMO then');
  code.Add('            break;');
  code.Add('        until not _RegEx.ExecNext;');
  code.Add('      end;');
  code.Add('    finally');
  code.Add('      _RegEx.Free;');
  code.Add('    end;');
  code.Add('  end;');
  code.Add('end;');

  memBody.Lines.Assign(code);

  code.Free;
end;

function TformCode.GenConstExpressionDef(AParentNode: PVirtualNode;
  var AIndex: Integer; out AVarName : String; ACode: TStrings) : Integer;
var
  Node : PVirtualNode;
  E : TExpression;
  s : String;
  childcount : Integer;
  children_varname : String;

  function GenVarName(AIdx : Integer) : String;
  begin Result := '_' + IntToStr(AIdx); end;
begin
  Result := 0;

  Node := formMain.TVExpressions.GetFirstChild(AParentNode);

  AVarName := GenVarName(AIndex);
  inc(AIndex);

  while Assigned(Node) do
  begin
    if formMain.TVExpressions.CheckState[Node] = csCheckedNormal then
    begin
      E := TExpression(TExpression.GetNodeData(Node, formMain.TVExpressions));

      if Result > 0 then
        s := s + ',';

      s := s + Format('(E:''%s''; RS:''%s''; CS:''%s''; MG:%s; MI:%s; MM:%s; MR:%s; MS:%s; MX:%s; FMO:%s; children:',
                      [StringReplace(e.Expression, '''', '''''', [rfReplaceAll]),
                       StringReplace(e.ReturnSubstitution, '''', '''''', [rfReplaceAll]),
                       StringReplace(e.ChildSubstitution, '''', '''''', [rfReplaceAll]),
                       BoolToStr(e.ModifierG, true), BoolToStr(e.ModifierI, true),
                       BoolToStr(e.ModifierM, true), BoolToStr(e.ModifierR, true),
                       BoolToStr(e.ModifierS, true), BoolToStr(e.ModifierX, true),
                       BoolToStr(e.FirstMatchOnly, true)]);

      childcount := GenConstExpressionDef(Node, AIndex, children_varname, ACode);

      if childcount > 0 then
        s := s + Format('@%s; childcount:%d', [children_varname, childcount])
      else
        s := s + 'nil';

      s := s + ')';

      inc(Result);
    end;

    Node := formMain.TVExpressions.GetNextSibling(Node);
  end;

  if Result > 0 then
    ACode.Add('  ' + AVarName + ' : array[0..' + IntToStr(Result - 1) + '] of _TExpression = (' + s + ');');
end;

procedure TformCode.memBodyEnter(Sender: TObject);
begin
  memBody.SelectAll;
end;

function TformCode.RE(AInputString: String; AMatchList: TStrings;
  AExpression: Pointer): Integer;
type
  _PExpressions = ^_TExpressions;
  _TExpression = record E, RS, CS : String; MG, MI, MM, MR, MS, MX : Boolean; Children : _PExpressions; end;
  _PExpression = ^_TExpression;
  _TExpressions = array of _TExpression;
const
  _0 : array[0..0] of _TExpression = ((E : 'das'));
var
  _Expr : _PExpression absolute AExpression;
  _ExprObj : _TExpression;
  _RegEx : TRegExpr;
  _Subst : String;
begin
  Result := 0;

  if not Assigned(_Expr) then
  begin
    for _ExprObj in _0 do
      Inc(Result, RE(AInputString, AMatchList, @_ExprObj));
  end
  else
  begin
    _RegEx := TRegExpr.Create;
    try
      _RegEx.Expression := _Expr^.E;
      _RegEx.ModifierG := _Expr^.MG; _RegEx.ModifierI := _Expr^.MI; _RegEx.ModifierM := _Expr^.MM;
      _RegEx.ModifierR := _Expr^.MR; _RegEx.ModifierS := _Expr^.MS; _RegEx.ModifierX := _Expr^.MX;

      if _RegEx.Exec(AInputString) then
      begin
        repeat
          if _Expr^.RS <> '' then
          begin
            AMatchList.Add(_RegEx.Substitute(_Expr^.RS));
            Inc(Result);
          end;

          if _Expr^.CS <> '' then
          begin
            _Subst := _RegEx.Substitute(_Expr^.CS);
            for _ExprObj in _Expr^.Children^ do
              Inc(Result, RE(_Subst, AMatchList, @_ExprObj));
          end;

        until not _RegEx.ExecNext;
      end;
    finally
      _RegEx.Free;
    end;
  end;
end;

function TformCode.RE3(AInputString, ADefaultResult: String;
  AExpression: Pointer): String;
type
  _PExpressions = ^_TExpressions;
  _TExpression = record E, RS, CS : String; MG, MI, MM, MR, MS, MX : Boolean; Children : _PExpressions; end;
  _PExpression = ^_TExpression;
  _TExpressions = array of _TExpression;
const
  _0 : array[0..0] of _TExpression = ((E : 'das'));
var
  _Expr : _PExpression absolute AExpression;
  _ExprObj : _TExpression;
  _RegEx : TRegExpr;
  _Subst : String;
begin
  Result := '';

  if not Assigned(_Expr) then
  begin
    for _ExprObj in _0 do
      Result := Result + RE3(AInputString, ADefaultResult, @_ExprObj);
  end
  else
  begin
    _RegEx := TRegExpr.Create;
    try
      _RegEx.Expression := _Expr^.E;
      _RegEx.ModifierG := _Expr^.MG; _RegEx.ModifierI := _Expr^.MI; _RegEx.ModifierM := _Expr^.MM;
      _RegEx.ModifierR := _Expr^.MR; _RegEx.ModifierS := _Expr^.MS; _RegEx.ModifierX := _Expr^.MX;

      if _RegEx.Exec(AInputString) then
      begin
        repeat
          if _Expr^.RS <> '' then
          begin
            Result := Result + _RegEx.Substitute(_Expr^.RS);
          end;

          if _Expr^.CS <> '' then
          begin
            _Subst := _RegEx.Substitute(_Expr^.CS);
            for _ExprObj in _Expr^.Children^ do
              Result := Result + RE3(_Subst, ADefaultResult, @_ExprObj);
          end;

        until not _RegEx.ExecNext;
      end;
    finally
      _RegEx.Free;
    end;
  end;
end;


end.
