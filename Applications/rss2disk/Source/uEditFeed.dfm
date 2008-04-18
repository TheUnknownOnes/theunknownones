object form_EditFeed: Tform_EditFeed
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit feed'
  ClientHeight = 202
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_Label1: TLabel
    Left = 8
    Top = 11
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lbl_Label2: TLabel
    Left = 8
    Top = 38
    Width = 67
    Height = 13
    Caption = 'Source (URL):'
  end
  object lbl_Label3: TLabel
    Left = 8
    Top = 65
    Width = 41
    Height = 11
    Caption = 'Save to:'
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 171
    Width = 458
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 330
    ExplicitWidth = 497
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 299
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 335
      ExplicitTop = 8
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 380
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 416
      ExplicitTop = 8
    end
  end
  object ed_Name: TEdit
    Left = 81
    Top = 8
    Width = 216
    Height = 21
    TabOrder = 1
  end
  object ed_Source: TEdit
    Left = 81
    Top = 35
    Width = 368
    Height = 21
    TabOrder = 2
  end
  object ed_SaveTo: TJvDirectoryEdit
    Left = 81
    Top = 62
    Width = 368
    Height = 21
    DialogKind = dkWin32
    TabOrder = 3
  end
  object gb_Options: TGroupBox
    Left = 8
    Top = 104
    Width = 442
    Height = 57
    Caption = 'Options'
    TabOrder = 4
    object cb_RenameFile: TCheckBox
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Rename files to title'
      TabOrder = 0
    end
  end
end
