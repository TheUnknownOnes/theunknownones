inherited FormCNAPropEdSet: TFormCNAPropEdSet
  Width = 374
  Caption = 'FormCNAPropEdSet'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited panBottom: TPanel
    Width = 356
    inherited btnCancel: TButton
      Left = 247
    end
    inherited btnOK: TButton
      Left = 130
    end
    inherited btnClearValue: TButton
      Left = 1
    end
  end
  inherited panPropName: TPanel
    Width = 356
    TabOrder = 2
  end
  object cList: TCheckListBox
    Left = 0
    Top = 33
    Width = 356
    Height = 215
    Align = alClient
    Columns = 2
    ItemHeight = 13
    TabOrder = 1
  end
end
