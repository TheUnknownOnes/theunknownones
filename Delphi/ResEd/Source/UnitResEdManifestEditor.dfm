object FormResEdManifestEditor: TFormResEdManifestEditor
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'FormResEdManifestEditor'
  ClientHeight = 203
  ClientWidth = 459
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 82
    Height = 13
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Application Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 108
    Height = 13
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Application Description'
  end
  object Label3: TLabel
    Left = 8
    Top = 83
    Width = 130
    Height = 13
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Requested Execution Level'
  end
  object edAppName: TEdit
    Left = 144
    Top = 8
    Width = 201
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
  end
  object edAppDesc: TEdit
    Left = 144
    Top = 35
    Width = 305
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 374
    Top = 171
    Width = 75
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Save'
    ModalResult = 1
    TabOrder = 2
  end
  object cbUIA: TCheckBox
    Left = 144
    Top = 107
    Width = 97
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'UI Access'
    TabOrder = 3
  end
  object cbREL: TComboBox
    Left = 144
    Top = 80
    Width = 201
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = 'asInvoker'
    Items.Strings = (
      'asInvoker'
      'highestAvailable'
      'requireAdministrator')
  end
  object cbWin7Compatibility: TCheckBox
    Left = 144
    Top = 124
    Width = 201
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Application is Windows 7 compatible'
    TabOrder = 5
  end
  object cbDPIAware: TCheckBox
    Left = 144
    Top = 142
    Width = 201
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Application is dpi aware'
    TabOrder = 6
  end
end
