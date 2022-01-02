object FormSM9: TFormSM9
  Left = 232
  Top = 181
  Width = 979
  Height = 563
  Caption = 'SM9 Test'
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
  object pgcSM9: TPageControl
    Left = 8
    Top = 8
    Width = 953
    Height = 513
    ActivePage = tsFP2
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsFP2: TTabSheet
      Caption = 'FP2'
      object grpFP2: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'FP2'
        TabOrder = 0
        object btnTestFP2: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Test FP2'
          TabOrder = 0
          OnClick = btnTestFP2Click
        end
        object mmoFP2: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
    object tsFP4: TTabSheet
      Caption = 'FP4'
      ImageIndex = 1
      object grpFP4: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'FP4'
        TabOrder = 0
        object btnTestFp4: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Test FP4'
          TabOrder = 0
          OnClick = btnTestFp4Click
        end
        object mmoFP4: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
    object tsFP12: TTabSheet
      Caption = 'FP12'
      ImageIndex = 2
      object grpFP12: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'FP12'
        TabOrder = 0
        object btnTestFP12: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Test FP12'
          TabOrder = 0
          OnClick = btnTestFP12Click
        end
        object mmoFP12: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
    object tsAffinePoint: TTabSheet
      Caption = 'FP2 Affine Point'
      ImageIndex = 3
      object grpAP: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'FP2 Affine Point'
        TabOrder = 0
        object btnAP: TButton
          Left = 16
          Top = 24
          Width = 121
          Height = 25
          Caption = 'Test FP2 Affine Point'
          TabOrder = 0
          OnClick = btnAPClick
        end
        object mmoAP: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 2
        end
        object btnFP2PointMul: TButton
          Left = 160
          Top = 24
          Width = 75
          Height = 25
          Caption = 'FP2 Point Mul'
          TabOrder = 1
          OnClick = btnFP2PointMulClick
        end
      end
    end
    object tsRate: TTabSheet
      Caption = 'R-ate'
      ImageIndex = 4
      object grpRate: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'R-ate Pairing'
        TabOrder = 0
        object btnRateTest: TButton
          Left = 16
          Top = 24
          Width = 89
          Height = 25
          Caption = 'Test R-ate'
          TabOrder = 0
          OnClick = btnRateTestClick
        end
        object mmoRate: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object btnRateTime: TButton
          Left = 120
          Top = 24
          Width = 89
          Height = 25
          Caption = 'R-ate Time'
          TabOrder = 2
          OnClick = btnRateTimeClick
        end
      end
    end
    object tsSM9Hash: TTabSheet
      Caption = 'SM9 Hash'
      ImageIndex = 5
      object grpSM9Hash: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'SM9 Hash'
        TabOrder = 0
        object btnTestHash: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Test Hash1'
          TabOrder = 0
          OnClick = btnTestHashClick
        end
        object btnTestHash2: TButton
          Left = 112
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Test Hash2'
          TabOrder = 1
          OnClick = btnTestHash2Click
        end
      end
    end
    object tsSM9Sign: TTabSheet
      Caption = 'SM9 Sign'
      ImageIndex = 6
      object grpSM9Sign: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'SM9 Hash'
        TabOrder = 0
        object lblUserID: TLabel
          Left = 320
          Top = 28
          Width = 39
          Height = 13
          Caption = 'User ID:'
        end
        object bvl1: TBevel
          Left = 472
          Top = 16
          Width = 9
          Height = 41
          Shape = bsLeftLine
        end
        object lbl1: TLabel
          Left = 520
          Top = 28
          Width = 26
          Height = 13
          Caption = 'Data:'
        end
        object btnSM9GenMaster: TButton
          Left = 16
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Generate Master Key'
          TabOrder = 0
          OnClick = btnSM9GenMasterClick
        end
        object btnSM9GenUser: TButton
          Left = 168
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Generate User Key'
          TabOrder = 1
          OnClick = btnSM9GenUserClick
        end
        object edtSigUserId: TEdit
          Left = 368
          Top = 24
          Width = 97
          Height = 21
          TabOrder = 2
          Text = 'CnPack Team'
        end
        object mmoSig: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 7
        end
        object edtSignData: TEdit
          Left = 560
          Top = 24
          Width = 177
          Height = 21
          TabOrder = 4
          Text = 'Message to Sign'
        end
        object btnSM9Sign: TButton
          Left = 752
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sign Data'
          TabOrder = 5
          OnClick = btnSM9SignClick
        end
        object btnSM9VerifyData: TButton
          Left = 840
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Verify Data'
          TabOrder = 6
          OnClick = btnSM9VerifyDataClick
        end
        object btnSM9Sample: TButton
          Left = 488
          Top = 24
          Width = 25
          Height = 25
          Caption = 'Tst'
          TabOrder = 3
          OnClick = btnSM9SampleClick
        end
      end
    end
    object tsSM9KeyEnc: TTabSheet
      Caption = 'SM9 Enc'
      ImageIndex = 7
      object grpKeyEnc: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Key Encapsulation'
        TabOrder = 0
        object lblKeyLength: TLabel
          Left = 536
          Top = 28
          Width = 86
          Height = 13
          Caption = 'Key Bytes Length:'
        end
        object lbl2: TLabel
          Left = 328
          Top = 28
          Width = 39
          Height = 13
          Caption = 'User ID:'
        end
        object bvl2: TBevel
          Left = 488
          Top = 16
          Width = 9
          Height = 41
          Shape = bsLeftLine
        end
        object bvl3: TBevel
          Left = 16
          Top = 216
          Width = 897
          Height = 25
          Shape = bsTopLine
        end
        object edtKeyEncLength: TEdit
          Left = 640
          Top = 24
          Width = 105
          Height = 21
          TabOrder = 4
          Text = '64'
        end
        object btnSM9KeyEncSend: TButton
          Left = 752
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Send'
          TabOrder = 5
          OnClick = btnSM9KeyEncSendClick
        end
        object mmoKeyEnc: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 129
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 7
        end
        object btnKeyEncGenMaster: TButton
          Left = 168
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Generate User Key'
          TabOrder = 1
          OnClick = btnKeyEncGenMasterClick
        end
        object btnKeyEncGenUser: TButton
          Left = 16
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Generate Master Key'
          TabOrder = 0
          OnClick = btnKeyEncGenUserClick
        end
        object edtDestUser: TEdit
          Left = 376
          Top = 24
          Width = 97
          Height = 21
          TabOrder = 2
          Text = 'CnPack Team'
        end
        object btnTestKeyEnc: TButton
          Left = 504
          Top = 24
          Width = 25
          Height = 25
          Caption = 'Tst'
          TabOrder = 3
          OnClick = btnTestKeyEncClick
        end
        object btnSM9KeyEncRecv: TButton
          Left = 840
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Receive'
          TabOrder = 6
          OnClick = btnSM9KeyEncRecvClick
        end
        object btnTestEnc: TButton
          Left = 16
          Top = 232
          Width = 25
          Height = 25
          Caption = 'Tst'
          TabOrder = 8
          OnClick = btnTestEncClick
        end
        object mmoEnc: TMemo
          Left = 16
          Top = 272
          Width = 897
          Height = 169
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 9
        end
      end
    end
    object tsSM9KeyExchange: TTabSheet
      Caption = 'SM9 Key Exchange'
      ImageIndex = 8
      object grpKeyExchange: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 465
        Caption = 'SM9 Hash'
        TabOrder = 0
        object btnKeyExchangeTest: TButton
          Left = 16
          Top = 24
          Width = 73
          Height = 25
          Caption = 'Test Sample'
          TabOrder = 0
          OnClick = btnKeyExchangeTestClick
        end
        object mmoKeyExchange: TMemo
          Left = 16
          Top = 72
          Width = 897
          Height = 377
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
    end
  end
end
