object FormNetDecl: TFormNetDecl
  Left = 192
  Top = 108
  Width = 946
  Height = 597
  Caption = 'Net Declaration Test'
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
    Width = 899
    Height = 534
    ActivePage = tsIP
    Anchors = [akLeft, akTop, akRight, akBottom]
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
        TabOrder = 4
      end
      object mmoIPSniffer: TMemo
        Left = 16
        Top = 56
        Width = 860
        Height = 432
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
      object rbAll: TRadioButton
        Left = 584
        Top = 18
        Width = 49
        Height = 17
        Caption = 'All'
        Checked = True
        TabOrder = 6
        TabStop = True
      end
      object btnIPManual: TButton
        Left = 640
        Top = 16
        Width = 75
        Height = 21
        Caption = 'Manual Parse'
        TabOrder = 7
        OnClick = btnIPManualClick
      end
      object btnCheckSum: TButton
        Left = 720
        Top = 16
        Width = 75
        Height = 21
        Caption = 'Check Sum'
        TabOrder = 8
        OnClick = btnCheckSumClick
      end
    end
    object tsSSL: TTabSheet
      Caption = 'SSL/TLS'
      ImageIndex = 1
      object bvl1: TBevel
        Left = 520
        Top = 16
        Width = 25
        Height = 25
        Shape = bsLeftLine
      end
      object btnSSLListenStart: TButton
        Left = 16
        Top = 16
        Width = 137
        Height = 25
        Caption = 'Listen Start'
        TabOrder = 0
        OnClick = btnSSLListenStartClick
      end
      object btnSSLParseTest: TButton
        Left = 176
        Top = 16
        Width = 177
        Height = 25
        Caption = 'Parse ClientHello/ServerHello Test'
        TabOrder = 1
        OnClick = btnSSLParseTestClick
      end
      object mmoSSL: TMemo
        Left = 16
        Top = 56
        Width = 860
        Height = 432
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object btnSSLClient: TButton
        Left = 536
        Top = 16
        Width = 75
        Height = 25
        Caption = 'TLS Client to'
        TabOrder = 3
        OnClick = btnSSLClientClick
      end
      object edtTLSHost: TEdit
        Left = 624
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '104.238.140.188'
      end
      object edtTLSPort: TEdit
        Left = 752
        Top = 16
        Width = 33
        Height = 21
        TabOrder = 5
        Text = '443'
      end
    end
  end
end
