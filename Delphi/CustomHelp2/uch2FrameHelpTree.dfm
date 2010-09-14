object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object TreeView1: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 33
    Width = 445
    Height = 268
    Align = alClient
    Images = ch2Data.ch2Images16
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnAdvancedCustomDrawItem = TreeView1AdvancedCustomDrawItem
    OnCollapsed = TreeView1Expanded
    OnDblClick = TreeView1DblClick
    OnExpanded = TreeView1Expanded
    OnKeyPress = TreeView1KeyPress
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 55
      Height = 24
      Align = alLeft
      Caption = 'Keywords'
      Layout = tlCenter
      ExplicitHeight = 16
    end
    object cbKeywords: TComboBox
      AlignWithMargins = True
      Left = 64
      Top = 3
      Width = 384
      Height = 24
      Align = alClient
      ItemHeight = 16
      TabOrder = 0
      OnCloseUp = cbKeywordsCloseUp
      OnKeyPress = cbKeywordsKeyPress
    end
  end
end
