inherited FormCNAPropEdEnum: TFormCNAPropEdEnum
  Width = 421
  Caption = 'FormCNAPropEdEnum'
  PixelsPerInch = 96
  TextHeight = 13
  inherited panBottom: TPanel
    Width = 403
    inherited btnCancel: TButton
      Left = 289
    end
    inherited btnOK: TButton
      Left = 177
      Enabled = False
    end
    inherited btnClearValue: TButton
      Left = 1
    end
  end
  inherited panPropName: TPanel
    Width = 403
    TabOrder = 2
    inherited PaintBox1: TPaintBox
      Width = 403
    end
  end
  object rgEnum: TRadioGroup
    Left = 0
    Top = 33
    Width = 403
    Height = 218
    Align = alClient
    Caption = 'rgEnum'
    Columns = 2
    TabOrder = 1
    OnClick = rgEnumClick
  end
end
