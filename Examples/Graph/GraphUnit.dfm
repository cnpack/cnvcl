object FormGraph: TFormGraph
  Left = 192
  Top = 130
  Width = 928
  Height = 571
  Caption = 'Graph'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 897
    Height = 513
    ActivePage = tsDirected
    TabOrder = 0
    object tsDirected: TTabSheet
      Caption = 'Directed Graph'
      object mmoLoad: TMemo
        Left = 8
        Top = 16
        Width = 225
        Height = 457
        TabOrder = 0
      end
      object btnLoad: TButton
        Left = 248
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 1
        OnClick = btnLoadClick
      end
      object btnDumpVertex: TButton
        Left = 248
        Top = 72
        Width = 75
        Height = 25
        Caption = 'Dump Vertex'
        TabOrder = 2
        OnClick = btnDumpVertexClick
      end
      object btnBiCheck: TButton
        Left = 248
        Top = 120
        Width = 75
        Height = 25
        Caption = 'BiCheck'
        TabOrder = 3
        OnClick = btnBiCheckClick
      end
      object lstVertex: TListBox
        Left = 336
        Top = 16
        Width = 153
        Height = 457
        ItemHeight = 13
        TabOrder = 4
        OnClick = lstVertexClick
        OnDblClick = lstVertexDblClick
      end
      object mmoTravel: TMemo
        Left = 688
        Top = 16
        Width = 185
        Height = 457
        TabOrder = 5
      end
      object mmoEdge: TMemo
        Left = 504
        Top = 16
        Width = 169
        Height = 457
        TabOrder = 6
      end
    end
  end
end
