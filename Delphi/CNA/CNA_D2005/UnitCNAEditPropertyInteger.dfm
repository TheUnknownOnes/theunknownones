inherited FormCNAPropEdInteger: TFormCNAPropEdInteger
  Width = 372
  Height = 168
  Caption = 'FormCNAPropEdInteger'
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
    Width = 354
    TabOrder = 2
    inherited btnCancel: TButton
      Left = 245
      TabOrder = 1
    end
    inherited btnOK: TButton
      Left = 128
      TabOrder = 0
    end
  end
  inherited panPropName: TPanel
    Width = 354
    TabOrder = 3
    inherited PaintBox1: TPaintBox
      Width = 354
    end
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
