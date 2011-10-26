object ch2FrameHelpTree: Tch2FrameHelpTree
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  TabStop = True
  object TreeView1: TTreeView
    AlignWithMargins = True
    Left = 3
    Top = 33
    Width = 445
    Height = 249
    Align = alClient
    Images = ch2Data.ch2Images16
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnAdvancedCustomDrawItem = TreeView1AdvancedCustomDrawItem
    OnCollapsed = TreeView1Expanded
    OnCompare = TreeView1Compare
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
      Width = 54
      Height = 24
      Align = alLeft
      Caption = 'Search for:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object cbKeywords: TComboBox
      AlignWithMargins = True
      Left = 63
      Top = 3
      Width = 385
      Height = 21
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnCloseUp = cbKeywordsCloseUp
      OnKeyPress = cbKeywordsKeyPress
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 285
    Width = 451
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 208
    Top = 136
  end
end
