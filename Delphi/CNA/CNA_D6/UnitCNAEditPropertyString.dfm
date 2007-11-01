inherited FormCNAPropEdString: TFormCNAPropEdString
  Left = 497
  Top = 230
  Width = 386
  Height = 230
  Caption = 'FormCNAPropEdString'
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblOldValue: TLabel [0]
    Left = 8
    Top = 35
    Width = 52
    Height = 13
    Caption = 'lblOldValue'
  end
  object lblNewValue: TLabel [1]
    Left = 8
    Top = 83
    Width = 57
    Height = 13
    Caption = 'lblNewValue'
  end
  inherited panBottom: TPanel
    Top = 138
    Width = 368
    TabOrder = 2
    inherited btnCancel: TButton
      Left = 256
      TabOrder = 1
    end
    inherited btnOK: TButton
      Left = 139
      TabOrder = 0
    end
  end
  inherited panPropName: TPanel
    Width = 368
    TabOrder = 3
  end
  object edOldValue: TEdit
    Left = 8
    Top = 51
    Width = 353
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object edNewValue: TEdit
    Left = 8
    Top = 99
    Width = 353
    Height = 21
    TabOrder = 1
  end
end
