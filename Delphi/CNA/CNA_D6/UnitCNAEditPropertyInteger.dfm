inherited FormCNAPropEdInteger: TFormCNAPropEdInteger
  Width = 375
  Height = 171
  Caption = 'FormCNAPropEdInteger'
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblOldValue: TLabel [0]
    Left = 17
    Top = 40
    Width = 52
    Height = 13
    Caption = 'lblOldValue'
  end
  object lblNewValue: TLabel [1]
    Left = 193
    Top = 40
    Width = 57
    Height = 13
    Caption = 'lblNewValue'
  end
  inherited panBottom: TPanel
    Top = 79
    Width = 357
    TabOrder = 2
    inherited btnCancel: TButton
      Left = 248
      TabOrder = 1
    end
    inherited btnOK: TButton
      Left = 139
      TabOrder = 0
    end
  end
  inherited panPropName: TPanel
    Width = 357
    TabOrder = 3
  end
  object edOldValue: TEdit
    Left = 17
    Top = 56
    Width = 145
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object edNewValue: TEdit
    Left = 192
    Top = 56
    Width = 145
    Height = 21
    TabOrder = 0
    OnKeyPress = edNewValueKeyPress
  end
end
