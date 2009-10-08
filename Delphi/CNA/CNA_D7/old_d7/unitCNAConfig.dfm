object formConfig: TformConfig
  Left = 275
  Top = 123
  Width = 539
  Height = 674
  BorderIcons = [biSystemMenu]
  Caption = 'CNA-Config'
  Color = clBtnFace
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001001800680300001600000028000000100000002000
    0000010018000000000040030000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000D68200D68200D68200D68200
    D68200D68200D68200D68200D68200D68200D68200D682000000000000000000
    00D68200FFF7EDFFEDDAFFEBD5FFE9D0FFE7CCFFE5C6FFE3C2FFE0BFFFDFBBFF
    DEB8FFDDB6FFE2C2D68200000000000000D68200FFF2E6FEEAD2FEE7CEFEE5C9
    FEE3C4FEE0BFFEDEBBFEDCB7000000FED9B0000000FFDDB7D682000000000000
    00D68200FFF5EBFFEDD7FFEAD36F81EA3F5FF25F75E9DECBC8FEDFBBFFDDB700
    0000FED9B1FFDEB9D68200000000000000D68200FFF8F0FEEFDCFEECD7FEEAD3
    0033FFBEB8D7FEE3C5FEE1C0FEE0BC000000FEDBB5FFE0BCD682000000000000
    00D68200FFFBF5FEF1E2FFEEDCFFECD70033FFBFBBDAFEE5C9FEE3C5FEE1C100
    0000FEDDB8FFE1BFD68200000000000000D68200FFFDFAFFF3E7FEF1E2FFEFDD
    0033FFBEBDDEFFE8CFFFE5C9FEE4C5000000FEDFBCFFE3C4D682000000000000
    00D68200FFFFFEFFF6EBFFF3E7FEF1E20033FFBEBEE3FEEAD4FFE8CFFFE6CA00
    0000FEE1C1FFE5C9D68200000000000000D68200FFFFFFDFDFF2FFF6EBFFF3E7
    0033FFBFC0E6FEEDD9DED4D9FFE9D0000000FEE4C6FFE7CED682000000000000
    00D68200FFFFFF7F96FACFD2F3FFF5EB0033FFBFC2E9EEE3E05F78F1FFEBD400
    0000FFE6CAFFEAD2D68200000000000000D68200FFFFFF9EB0FA3F64FC3F63FB
    3F63FA3F63F93F62F86F85F1FEEDD9000000FFE8D0FFECD7D682000000000000
    00D68200FFFFFFFFFDFAFFFCF7FFFAF4FEF8F0FFF6ECFFF4E8FEF2E3000000FF
    EEDA000000FFEFDDD68200000000000000D68200FFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFEFCFFFCF7FFF9F3FFF7EDFFF4E8FFF9F2D682000000000000
    00000000D68200D68200D68200D68200D68200D68200D68200D68200D68200D6
    8200D68200D68200000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000C00300008001000080010000800100008001000080010000800100008001
    00008001000080010000800100008001000080010000C0030000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbOptions: TGroupBox
    Left = 0
    Top = 0
    Width = 531
    Height = 249
    Align = alTop
    Caption = 'gbOptions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object panExpertActive: TPanel
      Left = 2
      Top = 15
      Width = 527
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      DesignSize = (
        527
        24)
      object cbExpertActive: TCheckBox
        Left = 8
        Top = 4
        Width = 513
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'cbExpertActive'
        TabOrder = 0
      end
    end
    object panUseNA: TPanel
      Left = 2
      Top = 39
      Width = 527
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      DesignSize = (
        527
        24)
      object cbUseNA: TCheckBox
        Left = 24
        Top = 4
        Width = 497
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'cbUseNA'
        TabOrder = 0
      end
    end
    object panNAOptions: TPanel
      Left = 2
      Top = 63
      Width = 527
      Height = 91
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      DesignSize = (
        527
        91)
      object lblNADelimiter: TLabel
        Left = 40
        Top = 52
        Width = 65
        Height = 13
        Caption = 'lblNADelimiter'
      end
      object cbNAPopIfDefined: TCheckBox
        Left = 40
        Top = 4
        Width = 481
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'cbNAPopIfDefined'
        TabOrder = 0
      end
      object cbNACreateName: TCheckBox
        Left = 40
        Top = 28
        Width = 474
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'cbNACreateName'
        TabOrder = 1
      end
      object edNADelimiter: TEdit
        Left = 39
        Top = 65
        Width = 66
        Height = 21
        MaxLength = 5
        TabOrder = 2
      end
    end
    object panUsePA: TPanel
      Left = 2
      Top = 154
      Width = 527
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      DesignSize = (
        527
        24)
      object cbUsePA: TCheckBox
        Left = 24
        Top = 4
        Width = 497
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'cbUsePA'
        TabOrder = 0
      end
    end
    object panPAProfile: TPanel
      Left = 2
      Top = 201
      Width = 527
      Height = 44
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object lblProfile: TLabel
        Left = 23
        Top = 4
        Width = 40
        Height = 13
        Caption = 'lblProfile'
      end
      object comPAProfile: TComboBox
        Left = 23
        Top = 17
        Width = 346
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnClick = comPAProfileClick
      end
    end
    object panPAShowDLG: TPanel
      Left = 2
      Top = 178
      Width = 527
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      object cbPAShowDLG: TCheckBox
        Left = 40
        Top = 3
        Width = 473
        Height = 17
        Caption = 'cbPAShowDLG'
        TabOrder = 0
      end
    end
  end
  object gbSettings: TGroupBox
    Left = 0
    Top = 249
    Width = 531
    Height = 354
    Align = alClient
    Caption = 'gbSettings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object TV: TTreeView
      Left = 2
      Top = 15
      Width = 183
      Height = 337
      Align = alLeft
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HideSelection = False
      Indent = 25
      ParentFont = False
      PopupMenu = pumTV
      ReadOnly = True
      TabOrder = 0
      OnClick = TVClick
      OnDragDrop = TVDragDrop
      OnDragOver = TVDragOver
      OnKeyUp = TVKeyUp
    end
    object panSettingsClient: TPanel
      Left = 185
      Top = 15
      Width = 344
      Height = 337
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object panComponents: TPanel
        Left = 0
        Top = 0
        Width = 344
        Height = 97
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        DesignSize = (
          344
          97)
        object lblComponents: TLabel
          Left = 2
          Top = 2
          Width = 340
          Height = 13
          Align = alTop
          Caption = 'lblComponents'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object comComponents: TComboBox
          Left = 4
          Top = 16
          Width = 335
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnKeyUp = comComponentsKeyUp
        end
        object btnAddComponentGroup: TBitBtn
          Left = 45
          Top = 44
          Width = 252
          Height = 25
          Caption = 'btnAddComponentGroup'
          TabOrder = 1
          OnClick = btnAddComponentGroupClick
        end
        object btnAddComponentNew: TBitBtn
          Left = 45
          Top = 72
          Width = 252
          Height = 25
          Caption = 'btnAddComponentNew'
          TabOrder = 2
          OnClick = btnAddComponentNewClick
        end
      end
      object gbValues: TGroupBox
        Left = 0
        Top = 97
        Width = 344
        Height = 240
        Align = alClient
        Caption = 'gbValues'
        TabOrder = 1
        Visible = False
        object LV: TListView
          Left = 2
          Top = 15
          Width = 340
          Height = 223
          Align = alClient
          Columns = <
            item
              Width = 200
            end
            item
              Caption = 'D'
              MaxWidth = 16
              MinWidth = 16
              Width = 16
            end
            item
              AutoSize = True
            end>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ReadOnly = True
          RowSelect = True
          ParentFont = False
          SortType = stBoth
          TabOrder = 0
          ViewStyle = vsReport
          OnAdvancedCustomDrawItem = LVAdvancedCustomDrawItem
          OnAdvancedCustomDrawSubItem = LVAdvancedCustomDrawSubItem
          OnCompare = LVCompare
          OnDblClick = LVDblClick
          OnKeyDown = LVKeyDown
        end
      end
    end
  end
  object panFooter: TPanel
    Left = 0
    Top = 603
    Width = 531
    Height = 29
    Align = alBottom
    Anchors = [akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      531
      29)
    object btnOK: TBitBtn
      Left = 424
      Top = 2
      Width = 105
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'btnOK'
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
  object panCredits: TPanel
    Left = 0
    Top = 632
    Width = 531
    Height = 15
    Align = alBottom
    BevelOuter = bvLowered
    Caption = 
      'Developed by TheUnknownOnes [MarcoWarm@gmx.net | chaosben@web.de' +
      ']'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object pumTV: TPopupMenu
    Left = 96
    Top = 336
    object miAddProfile: TMenuItem
      OnClick = miAddProfileClick
    end
    object miAddGroup: TMenuItem
      OnClick = miAddGroupClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miRenameGroup: TMenuItem
      OnClick = miRenameGroupClick
    end
    object miRenameProfile: TMenuItem
      OnClick = miRenameProfileClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miDelComponent: TMenuItem
      Visible = False
      OnClick = miDelComponentClick
    end
    object miDelGroup: TMenuItem
      Visible = False
      OnClick = miDelGroupClick
    end
    object miDelProfile: TMenuItem
      Visible = False
      OnClick = miDelProfileClick
    end
    object miDelete: TMenuItem
      ShortCut = 46
      OnClick = miDeleteClick
    end
  end
end
