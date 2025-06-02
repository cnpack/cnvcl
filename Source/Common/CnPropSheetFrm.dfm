object CnPropSheetForm: TCnPropSheetForm
  Left = 432
  Top = 96
  Width = 459
  Height = 602
  BorderStyle = bsSizeToolWin
  Caption = 'CnDebug Inspector'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTree: TPanel
    Left = 0
    Top = 0
    Width = 100
    Height = 570
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object pnlSearch: TPanel
      Left = 0
      Top = 0
      Width = 100
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object btnSearch: TSpeedButton
        Left = 75
        Top = 4
        Width = 23
        Height = 22
        Hint = 'Search'
        Anchors = [akRight]
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FF00FF4A637B
          BD9494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FF6B9CC6188CE74A7BA5C69494FF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4AB5FF52B5FF
          218CEF4A7BA5C69494FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FF52B5FF52B5FF1884E74A7BA5C69494FF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          52B5FF4AB5FF188CE74A7BA5BD9494FF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B5FF4AB5FF2184DE5A6B73FF
          00FFAD7B73C6A59CD6B5A5CEA59CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF52BDFFB5D6EFA5948CB59C8CF7E7CEFFFFD6FFFFD6FFFFD6E7DE
          BDCEADA5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEB5B5D6B5A5FF
          EFC6FFFFD6FFFFD6FFFFD6FFFFDEFFFFEFF7F7EFB58C8CFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFC6948CF7DEB5F7D6A5FFF7CEFFFFD6FFFFDEFFFFEFFFFF
          F7FFFFFFDED6BDFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDEBDA5FFE7ADF7
          CE94FFF7CEFFFFDEFFFFE7FFFFF7FFFFF7FFFFEFF7EFD6C69C94FF00FFFF00FF
          FF00FFFF00FFFF00FFE7C6ADFFDEADEFBD84F7E7B5FFFFD6FFFFDEFFFFE7FFFF
          E7FFFFDEF7F7D6C6AD9CFF00FFFF00FFFF00FFFF00FFFF00FFDEBDADFFE7B5EF
          BD84F7CE94FFEFC6FFFFDEFFFFDEFFFFDEFFFFDEF7EFD6C6A59CFF00FFFF00FF
          FF00FFFF00FFFF00FFC69C94FFEFC6FFEFC6F7D6A5F7CE9CF7E7B5FFF7CEFFF7
          D6FFFFD6E7DEBDFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDEC6ADFF
          FFFFFFF7EFF7CE94EFBD84F7CE9CFFE7B5FFF7C6BD9C8CFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFD6BDBDF7EFD6FFEFC6FFE7ADFFE7B5F7DE
          B5CEAD9CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFCEAD94CEAD9CDEBDA5DEBDA5FF00FFFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnSearchClick
      end
      object edtSearch: TEdit
        Left = 3
        Top = 5
        Width = 70
        Height = 21
        Hint = 'Search Text'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnKeyPress = edtSearchKeyPress
      end
    end
    object TreeView: TTreeView
      Left = 0
      Top = 30
      Width = 100
      Height = 519
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnDblClick = TreeViewDblClick
    end
    object pnlTreeTab: TPanel
      Left = 0
      Top = 549
      Width = 100
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object pnlRight: TPanel
    Left = 100
    Top = 0
    Width = 351
    Height = 570
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 351
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object btnRefresh: TSpeedButton
        Left = 154
        Top = 4
        Width = 23
        Height = 22
        Hint = 'Refresh'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000C8D0D4C8D0D4
          C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0
          D4C8D0D4C8D0D4C8D0D4C8D0D4A37875A37875A37875A37875A37875A37875A3
          7875A37875A37875A37875A37875A3787590615EC8D0D4C8D0D4C8D0D4A67C76
          F2E2D3F2E2D3FFE8D1EFDFBBFFE3C5FFDEBDFFDDBAFFD8B2FFD6AEFFD2A5FFD2
          A3936460C8D0D4C8D0D4C8D0D4AB8078F3E7DAF3E7DA019901AFD8A071C57041
          AA3081BB5EEFD4A6FFD6AEFFD2A3FFD2A3966763C8D0D4C8D0D4C8D0D4B0837A
          F4E9DDF4E9DD01990101990101990101990101990141AA2FFFD8B2FFD4A9FFD4
          A99A6A65C8D0D4C8D0D4C8D0D4B6897DF5EDE4F5EDE4019901019901119E0ECF
          D6A3FFE4C821A21AFFD8B2FFD7B0FFD7B09E6D67C8D0D4C8D0D4C8D0D4BC8E7F
          F7EFE8F7EFE8019901019901019901019901FFE4C8EFDEBAFFD8B2FFD7B0FFD9
          B4A27069C8D0D4C8D0D4C8D0D4C39581F8F3EFF8F3EFF8F3EFFFF4E8FFF4E8FF
          F4E8EFE3C4EFE3C4FFE4C8FFDEBDFFDDBBA5746BC8D0D4C8D0D4C8D0D4CA9B84
          F9F5F2FBFBFBFFF4E8FFF4E8FFF4E8019901019901019901FFE8D1FFE3C5FFE1
          C2A8776DC8D0D4C8D0D4C8D0D4D2A187F9F9F9FBFBFB119F10AFD8A0FFF4E8AF
          D8A0019901019901FFE8D1FFE4C8FFE3C6AC7A6FC8D0D4C8D0D4C8D0D4D9A88A
          FBFBFBFFFFFF71C570019901019901019901019901019901FFE8D1FFE8D1FFE6
          CEAE7C72C8D0D4C8D0D4C8D0D4DFAE8CFCFCFCFFFFFFFFFFFF71C57001990101
          9901AFD8A0019901FFE8D1FFC8C2FFB0B0B07E73C8D0D4C8D0D4C8D0D4E5B38F
          FDFDFDFDFDFDFFFFFFFFFFFFFFFFFEFFFAF6FFF9F3FFF5EAF4DECEB28074B280
          74B28074C8D0D4C8D0D4C8D0D4EAB891FEFEFEFEFEFEFFFFFFFFFFFFFFFFFFFF
          FFFEFFFAF6FFF9F3F5E1D2B28074EDA755CAA38FC8D0D4C8D0D4C8D0D4EFBC92
          FFFFFFFFFFFFFCFCFCFAFAFAF7F7F7F5F5F5F2F1F1F0EDEAE9DAD0B28074D1AA
          92C9CECFC8D0D4C8D0D4C8D0D4F2BF94DCA987DCA987DCA987DCA987DCA987DC
          A987DCA987DCA987DCA987B28074CACECFC8D0D4C8D0D4C8D0D4}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnRefreshClick
      end
      object btnTop: TSpeedButton
        Left = 506
        Top = 5
        Width = 20
        Height = 21
        Hint = 'Always On Top'
        AllowAllUp = True
        Anchors = [akTop, akRight]
        GroupIndex = 1
        Caption = '^'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnTopClick
      end
      object lblDollar: TLabel
        Left = 20
        Top = 8
        Width = 6
        Height = 13
        Caption = '$'
      end
      object btnEvaluate: TSpeedButton
        Left = 130
        Top = 4
        Width = 23
        Height = 22
        Hint = 'Evaluate Address'
        Flat = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000C8D0D4C8D0D4
          C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0
          D4C8D0D4C8D0D4C8D0D4C8D0D484848400000000000000000000000000000000
          0000000000000000000000000000000000C8D0D4C8D0D4C8D0D4C8D0D4848484
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00C8D0D4C8D0D4C8D0D4C8D0D4848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000C8D0D4C8D0D4C8D0D4C8D0D4848484
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC6C6C6000000000000FFFFFFFFFFFF0000
          00C8D0D4C8D0D4C8D0D4C8D0D4848484FFFFFFFFFFFFFFFFFFFFFFFF84848400
          0000000000000000FFFFFFFFFFFF000000C8D0D4C8D0D4C8D0D4C8D0D4848484
          FFFFFFFFFFFFFFFFFF848484000000000000000000000000000000FFFFFF0000
          00C8D0D4C8D0D4C8D0D4C8D0D4848484FFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFF000000000000FFFFFF848484C8D0D4C8D0D4C8D0D4C8D0D4848484
          FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF000000000000C8D0
          D4C8D0D4C8D0D4C8D0D4C8D0D4848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF000000000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4848484
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00C8D0D4C8D0D4C8D0D4C8D0D4848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF848484848484C6C6C6000000000000C8D0D4C8D0D4C8D0D4848484
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF848484FFFFFF848484C8D0
          D4000000000000C8D0D4C8D0D4848484FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF848484848484C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4848484
          848484848484848484848484848484848484848484848484C8D0D4C8D0D4C8D0
          D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8
          D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4}
        ParentShowHint = False
        ShowHint = True
        OnClick = btnEvaluateClick
      end
      object btnLocate: TSpeedButton
        Left = 357
        Top = 5
        Width = 20
        Height = 21
        Hint = 'Locate Control'
        AllowAllUp = True
        Caption = '*'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = btnLocateClick
      end
      object btnTree: TSpeedButton
        Left = 2
        Top = 6
        Width = 17
        Height = 17
        Caption = '<'
        Flat = True
        OnClick = btnTreeClick
      end
      object edtObj: TEdit
        Left = 30
        Top = 5
        Width = 97
        Height = 21
        Hint = 'Object Address'
        TabOrder = 0
        Text = '00000000'
        OnKeyPress = edtObjKeyPress
      end
      object edtClassName: TEdit
        Left = 180
        Top = 8
        Width = 174
        Height = 18
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
        Text = 'Unknown'
      end
    end
    object pnlSwitchTab: TPanel
      Left = 0
      Top = 549
      Width = 351
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlMain: TPanel
      Left = 0
      Top = 30
      Width = 351
      Height = 519
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object lvProp: TListView
        Left = 8
        Top = 0
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Property Name'
            Width = 130
          end
          item
            Caption = 'Property Value'
            Width = 180
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 11
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnDblClick = lvPropDblClick
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object lvFields: TListView
        Left = 8
        Top = 0
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Field Name'
            Width = 130
          end
          item
            Caption = 'Field Value'
            Width = 180
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 9
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnDblClick = lvFieldsDblClick
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object mmoText: TMemo
        Left = 96
        Top = 352
        Width = 185
        Height = 49
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object lvEvent: TListView
        Left = 8
        Top = 64
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Event'
            Width = 130
          end
          item
            Caption = 'Handler'
            Width = 180
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 2
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object lvMethods: TListView
        Left = 16
        Top = 56
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Method Name'
            Width = 130
          end
          item
            Caption = 'Full Method'
            Width = 180
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 10
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnDblClick = lvPropDblClick
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object pnlInspectBtn: TPanel
        Left = 10
        Top = 10
        Width = 22
        Height = 22
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 3
        object btnInspect: TSpeedButton
          Left = 0
          Top = 0
          Width = 22
          Height = 22
          Caption = '?'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -8
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          OnClick = btnInspectClick
        end
      end
      object lvCollectionItem: TListView
        Left = 8
        Top = 136
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Items[]'
            Width = 180
          end
          item
            Caption = 'Item Value'
            Width = 130
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 4
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object lvMenuItem: TListView
        Left = 8
        Top = 136
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'MenuItems[]'
            Width = 180
          end
          item
            Caption = 'Item Value'
            Width = 130
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object lvComp: TListView
        Left = 8
        Top = 208
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Components[]'
            Width = 150
          end
          item
            Caption = 'Component'
            Width = 180
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 5
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object lvControl: TListView
        Left = 8
        Top = 280
        Width = 321
        Height = 57
        Columns = <
          item
            Caption = 'Controls[]'
            Width = 130
          end
          item
            Caption = 'Control'
            Width = 180
          end>
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        PopupMenu = pmSheet
        TabOrder = 6
        ViewStyle = vsReport
        OnCustomDrawItem = lvPropCustomDrawItem
        OnCustomDrawSubItem = lvPropCustomDrawSubItem
        OnKeyDown = ListViewKeyDown
        OnSelectItem = lvPropSelectItem
      end
      object pnlHierarchy: TPanel
        Left = 0
        Top = 0
        Width = 351
        Height = 519
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 7
      end
      object pnlGraphic: TPanel
        Left = 0
        Top = 0
        Width = 351
        Height = 519
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 8
        Visible = False
        object pnlGraphicInfo: TPanel
          Left = 1
          Top = 477
          Width = 349
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object lblGraphicInfo: TLabel
            Left = 16
            Top = 16
            Width = 3
            Height = 13
          end
          object lblPixel: TLabel
            Left = 184
            Top = 16
            Width = 3
            Height = 13
          end
        end
        object bxGraphic: TScrollBox
          Left = 1
          Top = 1
          Width = 349
          Height = 476
          Align = alClient
          BorderStyle = bsNone
          TabOrder = 1
          object pbGraphic: TPaintBox
            Left = 0
            Top = 0
            Width = 439
            Height = 400
            Anchors = [akLeft, akTop, akRight]
            OnMouseMove = pbGraphicMouseMove
            OnPaint = pbGraphicPaint
          end
        end
      end
    end
  end
  object pmSheet: TPopupMenu
    Left = 197
    Top = 351
    object Copy1: TMenuItem
      Caption = '&Copy'
      OnClick = Copy1Click
    end
    object CopyAll1: TMenuItem
      Caption = 'Copy &All'
      OnClick = CopyAll1Click
    end
  end
end
