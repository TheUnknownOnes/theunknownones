object ResEdDotNetEdit: TResEdDotNetEdit
  Left = 0
  Top = 0
  ActiveControl = MemoValue
  BorderStyle = bsSizeToolWin
  BorderWidth = 3
  Caption = 'DotNet Resource Editor'
  ClientHeight = 266
  ClientWidth = 553
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
  object MemoDesc: TMemo
    Left = 0
    Top = 68
    Width = 553
    Height = 40
    Align = alTop
    BevelInner = bvLowered
    BevelKind = bkSoft
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
  end
  object MemoValue: TMemo
    Left = 0
    Top = 129
    Width = 553
    Height = 104
    Align = alClient
    BevelInner = bvLowered
    BevelKind = bkSoft
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 553
    Height = 68
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 32
      Height = 13
      Caption = 'Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 0
      Top = 52
      Width = 55
      Height = 13
      Caption = 'Comment'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 0
      Top = 17
      Width = 28
      Height = 13
      Caption = 'Type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 0
      Top = 34
      Width = 59
      Height = 13
      Caption = 'MIMEType'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object LabelName: TLabel
      Left = 80
      Top = 0
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object LabelType: TLabel
      Left = 80
      Top = 17
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object LabelMime: TLabel
      Left = 80
      Top = 34
      Width = 50
      Height = 13
      Caption = 'MIMEType'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 108
    Width = 553
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object Label3: TLabel
      Left = 0
      Top = 6
      Width = 31
      Height = 13
      Caption = 'Value'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 233
    Width = 553
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      553
      33)
    object btnCancel: TButton
      Left = 478
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 397
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      ModalResult = 1
      TabOrder = 0
    end
  end
end
