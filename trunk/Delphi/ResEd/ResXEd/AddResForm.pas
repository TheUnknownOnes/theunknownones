//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit AddResForm;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, System.Resources;

type
  TAddResForm = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    cmbResType: System.Windows.Forms.ComboBox;
    Label1: System.Windows.Forms.Label;
    edtResValue: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    Button1: System.Windows.Forms.Button;
    Button2: System.Windows.Forms.Button;
    edtResName: System.Windows.Forms.TextBox;
    Label3: System.Windows.Forms.Label;
    numResValue: System.Windows.Forms.NumericUpDown;
    boolResValue: System.Windows.Forms.ComboBox;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure Button1_Click(sender: System.Object; e: System.EventArgs);
    procedure cmbResType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure numResValue_Leave(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict
  private
    procedure AddObjectToResource(Obj: TObject; ResName, ResXName: String);
  private
    procedure AddStringToResource(str, ResName, ResXName: String); protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    ///
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    ResXFile : string;
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TAddResForm))]

implementation

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TAddResForm.InitializeComponent;
type
  TArrayOfSystem_Object = array of System.Object;
  TArrayOfInteger = array of Integer;
begin
  Self.cmbResType := System.Windows.Forms.ComboBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.edtResValue := System.Windows.Forms.TextBox.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Button1 := System.Windows.Forms.Button.Create;
  Self.Button2 := System.Windows.Forms.Button.Create;
  Self.edtResName := System.Windows.Forms.TextBox.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.numResValue := System.Windows.Forms.NumericUpDown.Create;
  Self.boolResValue := System.Windows.Forms.ComboBox.Create;
  (System.ComponentModel.ISupportInitialize(Self.numResValue)).BeginInit;
  Self.SuspendLayout;
  // 
  // cmbResType
  // 
  Self.cmbResType.Cursor := System.Windows.Forms.Cursors.Default;
  Self.cmbResType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cmbResType.Items.AddRange(TArrayOfSystem_Object.Create('System.String', 
          'System.Boolean', 'System.Byte', 'System.Int32'));
  Self.cmbResType.Location := System.Drawing.Point.Create(8, 24);
  Self.cmbResType.Name := 'cmbResType';
  Self.cmbResType.Size := System.Drawing.Size.Create(344, 21);
  Self.cmbResType.TabIndex := 0;
  Include(Self.cmbResType.SelectedIndexChanged, Self.cmbResType_SelectedIndexChanged);
  // 
  // Label1
  // 
  Self.Label1.AutoSize := True;
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(84, 16);
  Self.Label1.TabIndex := 1;
  Self.Label1.Text := 'Resource Type:';
  // 
  // edtResValue
  // 
  Self.edtResValue.Location := System.Drawing.Point.Create(192, 72);
  Self.edtResValue.Name := 'edtResValue';
  Self.edtResValue.Size := System.Drawing.Size.Create(160, 20);
  Self.edtResValue.TabIndex := 2;
  Self.edtResValue.Text := '';
  // 
  // Label2
  // 
  Self.Label2.AutoSize := True;
  Self.Label2.Location := System.Drawing.Point.Create(192, 56);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(88, 16);
  Self.Label2.TabIndex := 3;
  Self.Label2.Text := 'Resource Value:';
  // 
  // Button1
  // 
  Self.Button1.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.Button1.FlatStyle := System.Windows.Forms.FlatStyle.System;
  Self.Button1.Location := System.Drawing.Point.Create(192, 112);
  Self.Button1.Name := 'Button1';
  Self.Button1.TabIndex := 3;
  Self.Button1.Text := 'OK';
  Include(Self.Button1.Click, Self.Button1_Click);
  // 
  // Button2
  // 
  Self.Button2.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.Button2.FlatStyle := System.Windows.Forms.FlatStyle.System;
  Self.Button2.Location := System.Drawing.Point.Create(277, 112);
  Self.Button2.Name := 'Button2';
  Self.Button2.TabIndex := 4;
  Self.Button2.Text := 'Cancel';
  // 
  // edtResName
  // 
  Self.edtResName.Location := System.Drawing.Point.Create(8, 72);
  Self.edtResName.Name := 'edtResName';
  Self.edtResName.Size := System.Drawing.Size.Create(168, 20);
  Self.edtResName.TabIndex := 1;
  Self.edtResName.Text := '';
  // 
  // Label3
  // 
  Self.Label3.AutoSize := True;
  Self.Label3.Location := System.Drawing.Point.Create(8, 56);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(89, 16);
  Self.Label3.TabIndex := 7;
  Self.Label3.Text := 'Resource Name:';
  // 
  // numResValue
  // 
  Self.numResValue.Location := System.Drawing.Point.Create(192, 72);
  Self.numResValue.Maximum := System.Decimal.Create(TArrayOfInteger.Create(255, 
          0, 0, 0));
  Self.numResValue.Name := 'numResValue';
  Self.numResValue.Size := System.Drawing.Size.Create(160, 20);
  Self.numResValue.TabIndex := 8;
  Include(Self.numResValue.Leave, Self.numResValue_Leave);
  // 
  // boolResValue
  // 
  Self.boolResValue.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.boolResValue.Items.AddRange(TArrayOfSystem_Object.Create('True', 'Fal' +
        'se'));
  Self.boolResValue.Location := System.Drawing.Point.Create(192, 72);
  Self.boolResValue.Name := 'boolResValue';
  Self.boolResValue.Size := System.Drawing.Size.Create(160, 21);
  Self.boolResValue.TabIndex := 9;
  Self.boolResValue.Visible := False;
  // 
  // TAddResForm
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(370, 144);
  Self.Controls.Add(Self.boolResValue);
  Self.Controls.Add(Self.numResValue);
  Self.Controls.Add(Self.Label3);
  Self.Controls.Add(Self.edtResName);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.edtResValue);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.Button2);
  Self.Controls.Add(Self.Button1);
  Self.Controls.Add(Self.cmbResType);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.Name := 'TAddResForm';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterParent;
  Self.Text := 'Add New Resource...';
  (System.ComponentModel.ISupportInitialize(Self.numResValue)).EndInit;
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TAddResForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TAddResForm.Create;
begin
  inherited Create;
  InitializeComponent;
  cmbResType.SelectedIndex := 0;
  boolResValue.SelectedIndex := 0;
end;

procedure TAddResForm.numResValue_Leave(sender: System.Object; e: System.EventArgs);
begin
  if numResValue.Value > numResValue.Maximum then
  begin
    MessageBox.Show('Maximum allwed value is: ' + numResValue.Maximum.ToString);
    numResValue.Focus;
  end
  else if numResValue.Value < numResValue.Minimum then
  begin
    MessageBox.Show('Minimum allwed value is: ' + numResValue.Minimum.ToString);
    numResValue.Focus;
  end;
end;

procedure TAddResForm.cmbResType_SelectedIndexChanged(sender: System.Object;
    e: System.EventArgs);
begin
  numResValue.Visible := False;
  edtResValue.Visible := False;
  boolResValue.Visible := False;


  case cmbResType.SelectedIndex of
    1: begin
         // System.Boolean
         boolResValue.Visible := True;
       end;
    2: begin
         // System.Byte
         numResValue.Visible := True;
         numResValue.Minimum := 0;
         numResValue.Maximum := System.Byte.MaxValue;
       end;
    3: begin
         // System.Int32
         numResValue.Visible := True;
         numResValue.Minimum := System.Int32.MinValue;
         numResValue.Maximum := System.Int32.MaxValue;
       end;
  else
    edtResValue.Visible := True;
  end;
end;

procedure  TAddResForm.AddObjectToResource(Obj: TObject; ResName, ResXName: String);
var
  reader : System.Resources.ResXResourceReader;
  Writer : System.Resources.ResXResourceWriter;

  enum   : IDictionaryEnumerator;
begin
  Reader:=ResXResourceReader.Create(ResXName);
  Writer:=System.Resources.ResXResourceWriter.Create(ResXName);
  enum:=reader.GetEnumerator;
  while enum.MoveNext do
  begin
    Writer.AddResource(enum.Key.ToString, enum.Value);
  end;

  Writer.AddResource(ResName, Obj);
  writer.Close;
end;

procedure  TAddResForm.AddStringToResource(str: String; ResName, ResXName: String);
var
  reader : System.Resources.ResXResourceReader;
  Writer : System.Resources.ResXResourceWriter;

  enum   : IDictionaryEnumerator;
begin
  Reader:=ResXResourceReader.Create(ResXName);
  Writer:=System.Resources.ResXResourceWriter.Create(ResXName);
  enum:=reader.GetEnumerator;
  while enum.MoveNext do
  begin
    Writer.AddResource(enum.Key.ToString, enum.Value);
  end;

  Writer.AddResource(ResName, str.ToString);
  writer.Close;
end;

procedure TAddResForm.Button1_Click(sender: System.Object; e: System.EventArgs);
var
  t: System.Type;
  ErrorStr: string;
  ObjToWrite: TObject;
begin
   t := System.Type.GetType(cmbResType.Text);

  if t = nil then
  begin
    MessageBox.Show('Type "' + cmbResType.Text + '" is not a valid type!',
      'Unknown Type Error',MessageBoxButtons.OK, MessageBoxIcon.Error);
    DialogResult := System.Windows.Forms.DialogResult.Cancel;
  end
  else
    try
      ObjToWrite:=Nil;

      if t = TypeOf(System.String) then
        AddStringToResource(edtResValue.Text, edtResName.Text, ResXFile)
      else if t = TypeOf(System.Int32) then
        ObjToWrite:=TObject(System.Int32.Parse(numResValue.Text))
      else if t = TypeOf(System.Boolean) then
        ObjToWrite:=TObject(System.Boolean.Parse(boolResValue.Text))
      else if t = TypeOf(System.Byte) then
        ObjToWrite:=TObject(System.Byte.Parse(numResValue.Text));

      if Assigned(ObjToWrite) then
      begin
        AddObjectToResource(ObjToWrite, edtResName.Text, ResXFile);
      end;

      DialogResult := System.Windows.Forms.DialogResult.OK;
    except
      if  numResValue.Visible then
        ErrorStr := numResValue.Text
      else if boolResValue.Visible then
        ErrorStr := boolResValue.Text
      else
        ErrorStr := edtResValue.Text;

      MessageBox.Show('"' + ErrorStr + '" is not a valid valut to type "'
        + cmbResType.Text + '"!', 'Bad Value Error',
        MessageBoxButtons.OK, MessageBoxIcon.Error);
      DialogResult := System.Windows.Forms.DialogResult.Cancel;
    end;
end;

end.
