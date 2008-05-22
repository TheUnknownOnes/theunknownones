object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ParentMode'
  ClientHeight = 215
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_Value: TLabel
    Left = 8
    Top = 192
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object btn_SetValueParent1: TButton
    Left = 8
    Top = 88
    Width = 114
    Height = 25
    Caption = 'Set Value Parent1'
    TabOrder = 0
    OnClick = btn_SetValueParent1Click
  end
  object btn_GetValueParent1: TButton
    Left = 8
    Top = 119
    Width = 114
    Height = 25
    Caption = 'Get Value Parent1'
    TabOrder = 1
    OnClick = btn_GetValueParent1Click
  end
  object btn_DeleteValueParent1: TButton
    Left = 8
    Top = 150
    Width = 114
    Height = 25
    Caption = 'Delete Value Parent1'
    TabOrder = 2
    OnClick = btn_DeleteValueParent1Click
  end
  object btn_SetValueChild1: TButton
    Left = 128
    Top = 88
    Width = 114
    Height = 25
    Caption = 'Set Value Child1'
    TabOrder = 3
    OnClick = btn_SetValueChild1Click
  end
  object btn_GetValueChild1: TButton
    Left = 128
    Top = 119
    Width = 114
    Height = 25
    Caption = 'Get Value Child1'
    TabOrder = 4
    OnClick = btn_GetValueChild1Click
  end
  object btn_DeleteValueChild1: TButton
    Left = 128
    Top = 150
    Width = 114
    Height = 25
    Caption = 'Delete Value Child1'
    TabOrder = 5
    OnClick = btn_DeleteValueChild1Click
  end
  object rb_ModeAddsMissing: TRadioButton
    Left = 8
    Top = 8
    Width = 121
    Height = 17
    Caption = 'pmAddsMissing'
    Checked = True
    TabOrder = 6
    TabStop = True
    OnClick = rb_ModeAddsMissingClick
  end
  object rb_OverridesAll: TRadioButton
    Left = 8
    Top = 31
    Width = 121
    Height = 17
    Caption = 'pmOverridesAll'
    TabOrder = 7
    OnClick = rb_ModeAddsMissingClick
  end
  object rb_DontUse: TRadioButton
    Left = 8
    Top = 54
    Width = 121
    Height = 17
    Caption = 'pmDontUse'
    TabOrder = 8
    OnClick = rb_ModeAddsMissingClick
  end
  object ed_Value: TEdit
    Left = 44
    Top = 189
    Width = 198
    Height = 21
    TabOrder = 9
  end
  object ChildSettings: TSettingsFile
    ParentSettings = ParentSettings
    Left = 168
    Top = 48
  end
  object ParentSettings: TSettingsXMLFile
    Left = 168
    Top = 8
  end
end
