object FormMarkDown: TFormMarkDown
  Left = 192
  Top = 107
  Width = 1106
  Height = 680
  Caption = 'Test MarkDown'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgcMarkDown: TPageControl
    Left = 0
    Top = 0
    Width = 1098
    Height = 648
    ActivePage = tsParser
    Align = alClient
    TabOrder = 0
    object tsParser: TTabSheet
      Caption = 'Parser / RTF'
      object lblFontSize: TLabel
        Left = 288
        Top = 12
        Width = 74
        Height = 13
        Caption = 'Base Font Size:'
      end
      object mmoMarkDown: TMemo
        Left = 16
        Top = 48
        Width = 577
        Height = 281
        Lines.Strings = (
          '# Hea*er1*'
          '## Header2'
          '* '#35748#23450#20070
          '* '#30340#35828#27861#26041#24335
          '### Header3'
          #21507'`'#20102'`'#22043#20108#27004'*'#20154#24037'*'#32819'**'#20799#31185'**  '
          '---'
          #38500'~~'#38750#38463'~~'#21457#38142#25509#65306'[CnPack](https://www.cnpack.org)  '
          'Internal Help ![test]()'
          '1. '#21507#39277
          '2. '#21917#27700
          '>  * '#21917#27700
          '>  * '#21507#39277
          '> > '#19981#20449#21629'**'#23601#26159'**'#19981#20449#21629
          '```pascal'
          
            'function TCnRTFConverter.ConvertFragment(Fragment: TCnMarkDownTe' +
            'xtFragment): string;'
          'begin'
          '  if Fragment.FragmentType = cmfCodeBlock then'
          '    Result := EscapeContent(Fragment.Content)  // '#30452#25509#36716#20041#20869#23481#65292#19981#39069#22806#20462#39280
          '  else'
          '    Exit;'
          'end;'
          '```'
          
            '    FragmentType '#21917#27700' F**ragmentType FragmentType FragmentTypeFrag' +
            'mentType'#21507#39277'TypeFragmentType '
          'FragmentType Fragm**entType  '
          'FragmentT<https://cnpack.org>ypeFragmentType')
        TabOrder = 0
      end
      object redtMarkDown: TRichEdit
        Left = 632
        Top = 48
        Width = 433
        Height = 545
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          '')
        ParentFont = False
        TabOrder = 1
      end
      object btnTest: TButton
        Left = 984
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Test'
        TabOrder = 2
        OnClick = btnTestClick
      end
      object mmoParse: TMemo
        Left = 16
        Top = 344
        Width = 577
        Height = 249
        ScrollBars = ssVertical
        TabOrder = 3
      end
      object btnDump: TButton
        Left = 16
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Tokens Dump'
        TabOrder = 4
        OnClick = btnDumpClick
      end
      object btnParseTree: TButton
        Left = 104
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Parse Tree'
        TabOrder = 5
        OnClick = btnParseTreeClick
      end
      object btnConvRtf: TButton
        Left = 200
        Top = 8
        Width = 75
        Height = 25
        Caption = 'To RTF'
        TabOrder = 6
        OnClick = btnConvRtfClick
      end
      object btnShowRTF: TButton
        Left = 600
        Top = 496
        Width = 25
        Height = 25
        Caption = '->'
        TabOrder = 7
        OnClick = btnShowRTFClick
      end
      object edtBaseFontSize: TEdit
        Left = 368
        Top = 8
        Width = 49
        Height = 21
        TabOrder = 8
        Text = '12'
      end
    end
    object tsVirtual: TTabSheet
      Caption = 'Virtual View'
      object pnlVirtualTools: TPanel
        Left = 0
        Top = 0
        Width = 1098
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btnStartVirtualStream: TButton
          Left = 8
          Top = 6
          Width = 130
          Height = 25
          Caption = 'Start 1 MB stream'
          TabOrder = 0
          OnClick = btnStartVirtualStreamClick
        end
        object btnLoadVirtualHistory: TButton
          Left = 146
          Top = 6
          Width = 180
          Height = 25
          Caption = 'Load 10,000 / 50 MB'
          TabOrder = 1
          OnClick = btnLoadVirtualHistoryClick
        end
        object btnLoadVirtualSamples: TButton
          Left = 334
          Top = 6
          Width = 180
          Height = 25
          Caption = 'Load 12 Markdown samples'
          TabOrder = 2
          OnClick = btnLoadVirtualSamplesClick
        end
        object btnLoadVirtualPerformance: TButton
          Left = 522
          Top = 6
          Width = 190
          Height = 25
          Caption = 'Load 200 varied samples'
          TabOrder = 3
          OnClick = btnLoadVirtualPerformanceClick
        end
        object btnAppendVirtualUpdate: TButton
          Left = 720
          Top = 6
          Width = 220
          Height = 25
          Caption = 'Append random Markdown'
          TabOrder = 4
          OnClick = btnAppendVirtualUpdateClick
        end
      end
    end
  end
  object tmrVirtualStream: TTimer
    Enabled = False
    Interval = 33
    OnTimer = tmrVirtualStreamTimer
    Left = 536
    Top = 8
  end
end
