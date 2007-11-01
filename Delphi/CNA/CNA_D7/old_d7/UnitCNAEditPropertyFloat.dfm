inherited FormCNAPropEdFloat: TFormCNAPropEdFloat
  Width = 408
  Height = 168
  Caption = 'FormCNAPropEdFloat'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblOldValue: TLabel [0]
    Left = 8
    Top = 40
    Width = 52
    Height = 13
    Caption = 'lblOldValue'
  end
  object lblNewValue: TLabel [1]
    Left = 200
    Top = 40
    Width = 57
    Height = 13
    Caption = 'lblNewValue'
  end
  inherited panBottom: TPanel
    Top = 79
    Width = 390
    inherited btnCancel: TButton
      Left = 281
    end
    inherited btnOK: TButton
      Left = 164
    end
  end
  inherited panPropName: TPanel
    Width = 390
    TabOrder = 3
    inherited PaintBox1: TPaintBox
      Width = 390
    end
  end
  object edOldValue: TEdit
    Left = 8
    Top = 56
    Width = 169
    Height = 21
    TabOrder = 1
  end
  object edNewValue: TEdit
    Left = 200
    Top = 56
    Width = 169
    Height = 21
    TabOrder = 2
    OnKeyPress = edNewValueKeyPress
  end
end
