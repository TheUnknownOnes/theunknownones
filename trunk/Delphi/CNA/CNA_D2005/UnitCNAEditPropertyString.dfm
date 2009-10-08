inherited FormCNAPropEdString: TFormCNAPropEdString
  Width = 384
  Height = 211
  Caption = 'FormCNAPropEdString'
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
    Top = 122
    Width = 366
    TabOrder = 2
    inherited btnCancel: TButton
      Left = 257
      TabOrder = 1
    end
    inherited btnOK: TButton
      Left = 140
      TabOrder = 0
    end
  end
  inherited panPropName: TPanel
    Width = 366
    TabOrder = 3
    inherited PaintBox1: TPaintBox
      Width = 366
    end
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
