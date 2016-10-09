object FormNetDecl: TFormNetDecl
  Left = 192
  Top = 107
  Width = 864
  Height = 472
  Caption = 'Test Net Declarations'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgcNetDecl: TPageControl
    Left = 16
    Top = 16
    Width = 817
    Height = 409
    ActivePage = tsIP
    TabOrder = 0
    object tsIP: TTabSheet
      Caption = 'IP'
      object lblLocal: TLabel
        Left = 16
        Top = 18
        Width = 69
        Height = 13
        Caption = 'Local Adapter:'
      end
      object lblIPCount: TLabel
        Left = 752
        Top = 20
        Width = 3
        Height = 13
      end
      object cbbIP: TComboBox
        Left = 104
        Top = 16
        Width = 193
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object btnSniff: TButton
        Left = 320
        Top = 16
        Width = 75
        Height = 21
        Caption = 'Sniff'
        TabOrder = 1
        OnClick = btnSniffClick
      end
      object rbTCP: TRadioButton
        Left = 416
        Top = 18
        Width = 49
        Height = 17
        Caption = 'TCP'
        TabOrder = 2
      end
      object rbUDP: TRadioButton
        Left = 472
        Top = 18
        Width = 49
        Height = 17
        Caption = 'UDP'
        TabOrder = 3
      end
      object rbICMP: TRadioButton
        Left = 528
        Top = 18
        Width = 49
        Height = 17
        Caption = 'ICMP'
        Checked = True
        TabOrder = 4
        TabStop = True
      end
      object mmoIPSniffer: TMemo
        Left = 16
        Top = 56
        Width = 777
        Height = 313
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
    end
  end
end
