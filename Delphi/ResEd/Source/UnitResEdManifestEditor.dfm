object FormResEdManifestEditor: TFormResEdManifestEditor
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'FormResEdManifestEditor'
  ClientHeight = 209
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 17
  object Label1: TLabel
    Left = 10
    Top = 14
    Width = 104
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Application Name'
  end
  object Label2: TLabel
    Left = 10
    Top = 50
    Width = 137
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Application Description'
  end
  object Label3: TLabel
    Left = 10
    Top = 109
    Width = 165
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Requested Execution Level'
  end
  object edAppName: TEdit
    Left = 188
    Top = 10
    Width = 263
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
  end
  object edAppDesc: TEdit
    Left = 188
    Top = 46
    Width = 399
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 484
    Top = 167
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Save'
    ModalResult = 1
    TabOrder = 2
  end
  object cbUIA: TCheckBox
    Left = 188
    Top = 140
    Width = 127
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'UI Access'
    TabOrder = 3
  end
  object cbREL: TComboBox
    Left = 188
    Top = 105
    Width = 263
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    ItemHeight = 17
    ItemIndex = 0
    TabOrder = 4
    Text = 'asInvoker'
    Items.Strings = (
      'asInvoker'
      'highestAvailable'
      'requireAdministrator')
  end
end
