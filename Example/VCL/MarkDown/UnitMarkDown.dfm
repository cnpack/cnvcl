object FormMarkDown: TFormMarkDown
  Left = 192
  Top = 107
  Width = 1106
  Height = 636
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
  object mmoMarkDown: TMemo
    Left = 16
    Top = 48
    Width = 577
    Height = 281
    Lines.Strings = (
      '# Hea*er1*'
      '## Header2'
      '* 认定书'
      '* 的说法方式'
      '### Header3'
      '吃`了`嘛二楼*人工*耳**儿科**  '
      '---'
      '除~~非阿~~发链接：[CnPack](https://www.cnpack.org)  '
      'Internal Help ![test]()'
      '1. 吃饭'
      '2. 喝水'
      '>  * 喝水'
      '>  * 吃饭'
      '> > 不信命**就是**不信命'
      ''
      
        '    FragmentType 喝水 F**ragmentType FragmentType FragmentTypeFr' +
        'agmentType吃饭TypeFragmentType '
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
    Left = 416
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
end
