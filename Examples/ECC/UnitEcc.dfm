object FormEcc: TFormEcc
  Left = 122
  Top = 106
  Width = 777
  Height = 528
  Caption = 'ECC Test'
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
    Left = 16
    Top = 16
    Width = 737
    Height = 465
    ActivePage = tsLucas
    TabOrder = 0
    object tsSimpleECC: TTabSheet
      Caption = 'Simple ECC'
      object lblPrivateKey: TLabel
        Left = 456
        Top = 196
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblPublicKey: TLabel
        Left = 584
        Top = 196
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblData: TLabel
        Left = 360
        Top = 236
        Width = 26
        Height = 13
        Caption = 'Data:'
      end
      object Bevel2: TBevel
        Left = 400
        Top = 320
        Width = 313
        Height = 17
        Shape = bsTopLine
      end
      object lblECDH: TLabel
        Left = 360
        Top = 312
        Width = 33
        Height = 13
        Caption = 'ECDH:'
      end
      object lblDHA: TLabel
        Left = 360
        Top = 352
        Width = 10
        Height = 13
        Caption = 'A:'
      end
      object lblDHB: TLabel
        Left = 360
        Top = 392
        Width = 10
        Height = 13
        Caption = 'B:'
      end
      object lblXb: TLabel
        Left = 376
        Top = 392
        Width = 13
        Height = 13
        Caption = 'Xb'
      end
      object lblXA: TLabel
        Left = 376
        Top = 352
        Width = 13
        Height = 13
        Caption = 'Xa'
      end
      object grpSimpleECC: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 161
        Caption = 'Simple E23(1,1) Ò²¾ÍÊÇ Y^2 = X^3 + X + 1 mod 23'
        TabOrder = 0
        object Bevel1: TBevel
          Left = 20
          Top = 64
          Width = 653
          Height = 17
          Shape = bsTopLine
        end
        object lblPX: TLabel
          Left = 24
          Top = 92
          Width = 20
          Height = 13
          Caption = 'P.X:'
        end
        object lblPY: TLabel
          Left = 112
          Top = 92
          Width = 20
          Height = 13
          Caption = 'P.Y:'
        end
        object lblAdd: TLabel
          Left = 216
          Top = 92
          Width = 42
          Height = 13
          Caption = '+     Q.X:'
        end
        object lblQY: TLabel
          Left = 328
          Top = 92
          Width = 21
          Height = 13
          Caption = 'Q.Y:'
        end
        object lblAddResult: TLabel
          Left = 518
          Top = 92
          Width = 3
          Height = 13
        end
        object lblMPX: TLabel
          Left = 24
          Top = 128
          Width = 20
          Height = 13
          Caption = 'P.X:'
        end
        object lblMPY: TLabel
          Left = 112
          Top = 128
          Width = 20
          Height = 13
          Caption = 'P.Y:'
        end
        object lblMul: TLabel
          Left = 216
          Top = 128
          Width = 29
          Height = 13
          Caption = '*      K'
        end
        object lblMResult: TLabel
          Left = 518
          Top = 128
          Width = 3
          Height = 13
        end
        object btnPOn: TSpeedButton
          Left = 194
          Top = 88
          Width = 16
          Height = 16
          Caption = '.'
          Flat = True
          OnClick = btnPOnClick
        end
        object btnMPOn: TSpeedButton
          Left = 194
          Top = 124
          Width = 16
          Height = 16
          Caption = '.'
          Flat = True
          OnClick = btnMPOnClick
        end
        object btnQOn: TSpeedButton
          Left = 410
          Top = 88
          Width = 16
          Height = 16
          Caption = '.'
          Flat = True
          OnClick = btnQOnClick
        end
        object btnTest1: TButton
          Left = 20
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test P+Q'
          TabOrder = 0
          OnClick = btnTest1Click
        end
        object btnTest0: TButton
          Left = 135
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 0+P'
          TabOrder = 1
          OnClick = btnTest0Click
        end
        object btnTestOn: TButton
          Left = 250
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test On Curve'
          TabOrder = 2
          OnClick = btnTestOnClick
        end
        object btnTestInverse: TButton
          Left = 366
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test Inverse'
          TabOrder = 3
          OnClick = btnTestInverseClick
        end
        object btnTest2P: TButton
          Left = 481
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 2*P'
          TabOrder = 4
          OnClick = btnTest2PClick
        end
        object btnTestMul: TButton
          Left = 596
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test n*P'
          TabOrder = 5
          OnClick = btnTestMulClick
        end
        object edtPX: TEdit
          Left = 56
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 6
          Text = '6'
        end
        object edtPY: TEdit
          Left = 144
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 7
          Text = '19'
        end
        object edtQX: TEdit
          Left = 272
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 8
          Text = '9'
        end
        object edtQY: TEdit
          Left = 360
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 9
          Text = '7'
        end
        object btnEqual: TButton
          Left = 432
          Top = 88
          Width = 65
          Height = 21
          Caption = '='
          TabOrder = 10
          OnClick = btnEqualClick
        end
        object edtMPX: TEdit
          Left = 56
          Top = 124
          Width = 49
          Height = 21
          TabOrder = 11
          Text = '9'
        end
        object edtMPY: TEdit
          Left = 144
          Top = 124
          Width = 49
          Height = 21
          TabOrder = 12
          Text = '7'
        end
        object btnMEqual: TButton
          Left = 432
          Top = 124
          Width = 65
          Height = 21
          Caption = '='
          TabOrder = 14
          OnClick = btnMEqualClick
        end
        object edtMK: TEdit
          Left = 272
          Top = 124
          Width = 49
          Height = 21
          TabOrder = 13
          Text = '3'
        end
      end
      object chtE2311: TChart
        Left = 0
        Top = 177
        Width = 345
        Height = 266
        BackWall.Brush.Color = clWhite
        BackWall.Brush.Style = bsClear
        Title.AdjustFrame = False
        Title.Text.Strings = (
          'TChart')
        Title.Visible = False
        BottomAxis.Automatic = False
        BottomAxis.AutomaticMaximum = False
        BottomAxis.AutomaticMinimum = False
        BottomAxis.Maximum = 23
        LeftAxis.Automatic = False
        LeftAxis.AutomaticMaximum = False
        LeftAxis.AutomaticMinimum = False
        LeftAxis.ExactDateTime = False
        LeftAxis.Increment = 1
        LeftAxis.LabelsSeparation = 0
        LeftAxis.Maximum = 23
        View3D = False
        BevelOuter = bvNone
        TabOrder = 1
        object pntsrsSeries1: TPointSeries
          Marks.ArrowLength = 0
          Marks.Visible = False
          SeriesColor = clRed
          ShowInLegend = False
          Pointer.Dark3D = False
          Pointer.Draw3D = False
          Pointer.HorizSize = 3
          Pointer.InflateMargins = False
          Pointer.Pen.Style = psDashDot
          Pointer.Pen.Visible = False
          Pointer.Style = psRectangle
          Pointer.VertSize = 3
          Pointer.Visible = True
          XValues.DateTime = False
          XValues.Name = 'X'
          XValues.Multiplier = 1
          XValues.Order = loAscending
          YValues.DateTime = False
          YValues.Name = 'Y'
          YValues.Multiplier = 1
          YValues.Order = loNone
        end
      end
      object btnNewKey: TButton
        Left = 360
        Top = 192
        Width = 75
        Height = 21
        Caption = 'New Key'
        TabOrder = 2
        OnClick = btnNewKeyClick
      end
      object edtPrivateKey: TEdit
        Left = 504
        Top = 192
        Width = 65
        Height = 21
        TabOrder = 3
      end
      object edtPublicKey: TEdit
        Left = 624
        Top = 192
        Width = 89
        Height = 21
        TabOrder = 4
      end
      object edtData: TEdit
        Left = 400
        Top = 232
        Width = 73
        Height = 21
        TabOrder = 5
        Text = '7'
      end
      object btnEncrypt: TButton
        Left = 504
        Top = 232
        Width = 75
        Height = 21
        Caption = 'Encrypt'
        TabOrder = 6
        OnClick = btnEncryptClick
      end
      object edtEncrypted: TEdit
        Left = 600
        Top = 232
        Width = 113
        Height = 21
        TabOrder = 7
      end
      object btnDecrypt: TButton
        Left = 360
        Top = 272
        Width = 75
        Height = 21
        Caption = 'Decrypt'
        TabOrder = 8
        OnClick = btnDecryptClick
      end
      object edtDecrypted: TEdit
        Left = 448
        Top = 272
        Width = 177
        Height = 21
        TabOrder = 9
      end
      object btnBatchVerify: TButton
        Left = 640
        Top = 272
        Width = 75
        Height = 21
        Caption = 'Batch Verify'
        TabOrder = 10
        OnClick = btnBatchVerifyClick
      end
      object edtDHXa: TEdit
        Left = 400
        Top = 348
        Width = 41
        Height = 21
        TabOrder = 11
        Text = '3'
      end
      object edtDHXb: TEdit
        Left = 400
        Top = 388
        Width = 41
        Height = 21
        TabOrder = 16
        Text = '5'
      end
      object btnCalcYb: TButton
        Left = 450
        Top = 388
        Width = 49
        Height = 21
        Caption = 'Calc Yb'
        TabOrder = 17
        OnClick = btnCalcYbClick
      end
      object btnCalcXA: TButton
        Left = 450
        Top = 348
        Width = 49
        Height = 21
        Caption = 'Calc Ya'
        TabOrder = 12
        OnClick = btnCalcXAClick
      end
      object edtDHYa: TEdit
        Left = 506
        Top = 348
        Width = 63
        Height = 21
        TabOrder = 13
      end
      object edtDHYb: TEdit
        Left = 506
        Top = 388
        Width = 63
        Height = 21
        TabOrder = 18
      end
      object btnDHBCK: TButton
        Left = 582
        Top = 388
        Width = 65
        Height = 21
        Caption = 'B Calc Key'
        TabOrder = 19
        OnClick = btnDHBCKClick
      end
      object btnDHACKey: TButton
        Left = 582
        Top = 348
        Width = 65
        Height = 21
        Caption = 'A Calc Key'
        TabOrder = 14
        OnClick = btnDHACKeyClick
      end
      object edtAKey: TEdit
        Left = 654
        Top = 348
        Width = 59
        Height = 21
        TabOrder = 15
      end
      object edtBKey: TEdit
        Left = 654
        Top = 388
        Width = 59
        Height = 21
        TabOrder = 20
      end
    end
    object tsInt64ECC: TTabSheet
      Caption = 'Intt64 ECC'
      ImageIndex = 1
      object grpGenEcc: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 401
        Caption = 'Generated Ecc'
        TabOrder = 0
        object lblEccY2: TLabel
          Left = 16
          Top = 28
          Width = 221
          Height = 13
          Caption = 'Y ^ 2 = X ^ 3    +                X +                 mod  '
        end
        object lblEccG: TLabel
          Left = 300
          Top = 28
          Width = 11
          Height = 13
          Caption = 'G:'
        end
        object lblEccOrder: TLabel
          Left = 416
          Top = 28
          Width = 51
          Height = 13
          Caption = 'with Order:'
        end
        object Bevel3: TBevel
          Left = 16
          Top = 64
          Width = 537
          Height = 17
          Shape = bsTopLine
        end
        object btnGenEcc: TButton
          Left = 568
          Top = 24
          Width = 107
          Height = 21
          Caption = 'Generate Small Ecc'
          TabOrder = 6
          OnClick = btnGenEccClick
        end
        object edtEccA: TEdit
          Left = 104
          Top = 24
          Width = 33
          Height = 21
          TabOrder = 0
          Text = '12'
        end
        object edtEccB: TEdit
          Left = 168
          Top = 24
          Width = 33
          Height = 21
          TabOrder = 1
          Text = '199'
        end
        object edtEccP: TEdit
          Left = 240
          Top = 24
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '73'
        end
        object edtEccGX: TEdit
          Left = 316
          Top = 24
          Width = 41
          Height = 21
          TabOrder = 3
          Text = '21'
        end
        object edtEccGY: TEdit
          Left = 364
          Top = 24
          Width = 41
          Height = 21
          TabOrder = 4
          Text = '21'
        end
        object edtEccOrder: TEdit
          Left = 480
          Top = 24
          Width = 73
          Height = 21
          TabOrder = 5
          Text = '61'
        end
        object btnCalcNG: TButton
          Left = 568
          Top = 56
          Width = 105
          Height = 21
          Caption = 'Calculate N*G'
          TabOrder = 7
          OnClick = btnCalcNGClick
        end
        object mmoGenECCPoints: TMemo
          Left = 568
          Top = 96
          Width = 105
          Height = 289
          ScrollBars = ssVertical
          TabOrder = 10
          WantReturns = False
          WordWrap = False
        end
        object chtEccInt64: TChart
          Left = 8
          Top = 72
          Width = 393
          Height = 321
          BackWall.Brush.Color = clWhite
          BackWall.Brush.Style = bsClear
          MarginBottom = 0
          MarginLeft = 0
          MarginRight = 2
          Title.AdjustFrame = False
          Title.Text.Strings = (
            'TChart')
          Title.Visible = False
          BottomAxis.Automatic = False
          BottomAxis.AutomaticMaximum = False
          BottomAxis.AutomaticMinimum = False
          BottomAxis.Maximum = 1000
          LeftAxis.Automatic = False
          LeftAxis.AutomaticMaximum = False
          LeftAxis.AutomaticMinimum = False
          LeftAxis.Maximum = 1000
          Legend.Visible = False
          View3D = False
          View3DWalls = False
          BevelOuter = bvNone
          TabOrder = 8
          object pntsrsSeries2: TPointSeries
            Marks.ArrowLength = 0
            Marks.Visible = False
            SeriesColor = clRed
            Pointer.Brush.Color = clRed
            Pointer.HorizSize = 1
            Pointer.InflateMargins = False
            Pointer.Pen.Visible = False
            Pointer.Style = psCircle
            Pointer.VertSize = 1
            Pointer.Visible = True
            XValues.DateTime = False
            XValues.Name = 'X'
            XValues.Multiplier = 1
            XValues.Order = loAscending
            YValues.DateTime = False
            YValues.Name = 'Y'
            YValues.Multiplier = 1
            YValues.Order = loNone
          end
        end
        object btnLeRanDe: TButton
          Left = 480
          Top = 88
          Width = 75
          Height = 25
          Caption = 'Legendre'
          TabOrder = 9
          OnClick = btnLeRanDeClick
        end
        object btnBNGXtoPoint: TButton
          Left = 472
          Top = 360
          Width = 75
          Height = 21
          Caption = 'BN G.X to Pt'
          TabOrder = 12
          OnClick = btnBNGXtoPointClick
        end
        object btnInt64GXtoPt: TButton
          Left = 480
          Top = 120
          Width = 75
          Height = 21
          Caption = 'G.X to Pt'
          TabOrder = 11
          OnClick = btnInt64GXtoPtClick
        end
      end
    end
    object tsECC: TTabSheet
      Caption = 'BigNumber ECC'
      ImageIndex = 2
      object grpBNEcc: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 409
        Caption = 'BigNumber Ecc'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object lblBNEqu: TLabel
          Left = 16
          Top = 28
          Width = 92
          Height = 13
          Caption = 'Y ^ 2 = X ^ 3    +    '
        end
        object lblBNGX: TLabel
          Left = 16
          Top = 124
          Width = 18
          Height = 13
          Caption = 'GX:'
        end
        object lblBNGY: TLabel
          Left = 16
          Top = 156
          Width = 18
          Height = 13
          Caption = 'GY:'
        end
        object lblBNEccOrder: TLabel
          Left = 16
          Top = 188
          Width = 29
          Height = 13
          Caption = 'Order:'
        end
        object Bevel4: TBevel
          Left = 16
          Top = 216
          Width = 665
          Height = 17
          Shape = bsTopLine
        end
        object lblBNEccB: TLabel
          Left = 16
          Top = 60
          Width = 19
          Height = 13
          Caption = 'X + '
        end
        object lblBNEccMod: TLabel
          Left = 16
          Top = 92
          Width = 20
          Height = 13
          Caption = 'mod'
        end
        object lblBNEccPrivateKey: TLabel
          Left = 176
          Top = 304
          Width = 42
          Height = 13
          Caption = 'Private: -'
        end
        object lblBNEccPublicKey: TLabel
          Left = 112
          Top = 304
          Width = 40
          Height = 13
          Caption = 'Public:  |'
        end
        object lblBNEccDataPoint: TLabel
          Left = 16
          Top = 376
          Width = 53
          Height = 13
          Caption = 'Data Point:'
          OnClick = lblBNEccDataPointClick
        end
        object edtBNEccA: TEdit
          Left = 104
          Top = 24
          Width = 577
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object edtBNEccB: TEdit
          Left = 56
          Top = 56
          Width = 625
          Height = 21
          TabOrder = 1
          Text = '7'
        end
        object edtBNEccP: TEdit
          Left = 56
          Top = 88
          Width = 625
          Height = 21
          TabOrder = 2
        end
        object edtBNEccGX: TEdit
          Left = 56
          Top = 120
          Width = 625
          Height = 21
          TabOrder = 3
        end
        object edtBNEccGY: TEdit
          Left = 56
          Top = 152
          Width = 625
          Height = 21
          TabOrder = 4
        end
        object edtBNEccOrder: TEdit
          Left = 56
          Top = 184
          Width = 625
          Height = 21
          TabOrder = 5
        end
        object btnBNEccInverseG: TButton
          Left = 16
          Top = 264
          Width = 75
          Height = 21
          Caption = 'Inverse G'
          TabOrder = 7
          OnClick = btnBNEccInverseGClick
        end
        object edtBNEccResult: TEdit
          Left = 16
          Top = 232
          Width = 665
          Height = 21
          TabOrder = 6
        end
        object btnBNEccInverseAdd: TButton
          Left = 98
          Top = 264
          Width = 75
          Height = 21
          Caption = 'Inverse G + G'
          TabOrder = 8
          OnClick = btnBNEccInverseAddClick
        end
        object btnBNEccGx2: TButton
          Left = 180
          Top = 264
          Width = 75
          Height = 21
          Caption = 'G + G'
          TabOrder = 9
          OnClick = btnBNEccGx2Click
        end
        object btnBNEccG2SubG: TButton
          Left = 344
          Top = 264
          Width = 75
          Height = 21
          Caption = 'G * 2 - G'
          TabOrder = 11
          OnClick = btnBNEccG2SubGClick
        end
        object btnBNEccGAddG: TButton
          Left = 262
          Top = 264
          Width = 75
          Height = 21
          Caption = 'G * 2'
          TabOrder = 10
          OnClick = btnBNEccGAddGClick
        end
        object btnBNEccGSubG: TButton
          Left = 426
          Top = 264
          Width = 75
          Height = 21
          Caption = 'G - G'
          TabOrder = 12
          OnClick = btnBNEccGSubGClick
        end
        object btnBNEccNG: TButton
          Left = 508
          Top = 264
          Width = 75
          Height = 21
          Caption = 'n * G'
          TabOrder = 13
          OnClick = btnBNEccNGClick
        end
        object btnBNEcc4G: TButton
          Left = 590
          Top = 264
          Width = 75
          Height = 21
          Caption = '4 * G vs G++++'
          TabOrder = 14
          OnClick = btnBNEcc4GClick
        end
        object btnBNEccNewKey: TButton
          Left = 16
          Top = 300
          Width = 75
          Height = 21
          Caption = 'New Key'
          TabOrder = 15
          OnClick = btnBNEccNewKeyClick
        end
        object edtBNEccPublicKey: TEdit
          Left = 16
          Top = 336
          Width = 665
          Height = 21
          TabOrder = 17
        end
        object edtBNEccPrivateKey: TEdit
          Left = 232
          Top = 300
          Width = 449
          Height = 21
          TabOrder = 16
        end
        object edtBNEccDataPoint: TEdit
          Left = 80
          Top = 372
          Width = 505
          Height = 21
          TabOrder = 18
        end
        object btnBNEccCrypt: TButton
          Left = 592
          Top = 372
          Width = 91
          Height = 21
          Caption = 'Encrypt/Decrypt'
          TabOrder = 19
          OnClick = btnBNEccCryptClick
        end
      end
    end
    object tsWrapData: TTabSheet
      Caption = 'BigNumber ECC'
      ImageIndex = 3
      object grpWrap: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 321
        Caption = 'BigNumber Ecc'
        TabOrder = 0
        object lblWrapData: TLabel
          Left = 16
          Top = 28
          Width = 26
          Height = 13
          Caption = 'Data:'
        end
        object bvl1: TBevel
          Left = 64
          Top = 96
          Width = 617
          Height = 17
          Shape = bsTopLine
        end
        object lblBNECDH: TLabel
          Left = 16
          Top = 88
          Width = 33
          Height = 13
          Caption = 'ECDH:'
        end
        object lblBNECDHA: TLabel
          Left = 16
          Top = 120
          Width = 10
          Height = 13
          Caption = 'A:'
        end
        object lblBNECDHB: TLabel
          Left = 16
          Top = 216
          Width = 10
          Height = 13
          Caption = 'B:'
        end
        object lblBNECDHXb: TLabel
          Left = 40
          Top = 216
          Width = 13
          Height = 13
          Caption = 'Xb'
        end
        object lblBNECDHXa: TLabel
          Left = 40
          Top = 120
          Width = 13
          Height = 13
          Caption = 'Xa'
        end
        object edtWrapData: TEdit
          Left = 56
          Top = 24
          Width = 449
          Height = 21
          TabOrder = 0
          Text = '123321123456734234'
        end
        object btnWrapData: TButton
          Left = 512
          Top = 24
          Width = 75
          Height = 21
          Caption = 'X to Point'
          TabOrder = 1
          OnClick = btnWrapDataClick
        end
        object edtWrapPoint: TEdit
          Left = 16
          Top = 56
          Width = 657
          Height = 21
          TabOrder = 3
        end
        object edtBNECDHXa: TEdit
          Left = 70
          Top = 116
          Width = 515
          Height = 21
          TabOrder = 4
          Text = '30954823978126165439807650540507823796164384908578170843'
        end
        object edtBNECDHXb: TEdit
          Left = 70
          Top = 212
          Width = 515
          Height = 21
          TabOrder = 9
          Text = '554978037890271980743250435089743764116321467834980754709'
        end
        object btnBNECDHYb: TButton
          Left = 600
          Top = 212
          Width = 75
          Height = 21
          Caption = 'Calc Yb'
          TabOrder = 10
          OnClick = btnBNECDHYbClick
        end
        object btnBNECDHYa: TButton
          Left = 600
          Top = 116
          Width = 75
          Height = 21
          Caption = 'Calc Ya'
          TabOrder = 5
          OnClick = btnBNECDHYaClick
        end
        object edtBNECDHA: TEdit
          Left = 70
          Top = 148
          Width = 515
          Height = 21
          TabOrder = 6
        end
        object edtBNECDHB: TEdit
          Left = 70
          Top = 244
          Width = 515
          Height = 21
          TabOrder = 11
        end
        object btnBNECDHBkey: TButton
          Left = 600
          Top = 244
          Width = 75
          Height = 21
          Caption = 'B Calc Key'
          TabOrder = 12
          OnClick = btnBNECDHBkeyClick
        end
        object btnBNECDHAKey: TButton
          Left = 600
          Top = 148
          Width = 75
          Height = 21
          Caption = 'A Calc Key'
          TabOrder = 7
          OnClick = btnBNECDHAKeyClick
        end
        object edtBNECDHResA: TEdit
          Left = 70
          Top = 180
          Width = 515
          Height = 21
          TabOrder = 8
        end
        object edtBNECDHResB: TEdit
          Left = 70
          Top = 276
          Width = 515
          Height = 21
          TabOrder = 13
        end
        object btnTestECDH: TButton
          Left = 600
          Top = 276
          Width = 75
          Height = 21
          Caption = 'ECDH Sample'
          TabOrder = 14
          OnClick = btnTestECDHClick
        end
        object btnBNEccWrapRange: TButton
          Left = 600
          Top = 24
          Width = 75
          Height = 21
          Caption = 'Range to Point'
          TabOrder = 2
          OnClick = btnBNEccWrapRangeClick
        end
      end
    end
    object tsLucas: TTabSheet
      Caption = 'Lucas Sequence'
      ImageIndex = 4
      object grpLucas: TGroupBox
        Left = 16
        Top = 16
        Width = 697
        Height = 409
        Caption = 'Lucas Sequence'
        TabOrder = 0
        object lblLucasX: TLabel
          Left = 16
          Top = 28
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lblLucasY: TLabel
          Left = 112
          Top = 28
          Width = 10
          Height = 13
          Caption = 'Y:'
        end
        object lblLucasP: TLabel
          Left = 208
          Top = 28
          Width = 10
          Height = 13
          Caption = 'P:'
        end
        object edtLucasX: TEdit
          Left = 32
          Top = 24
          Width = 65
          Height = 21
          TabOrder = 0
          Text = '2'
        end
        object edtLucasY: TEdit
          Left = 128
          Top = 24
          Width = 57
          Height = 21
          TabOrder = 1
          Text = '3'
        end
        object edtLucasP: TEdit
          Left = 224
          Top = 24
          Width = 49
          Height = 21
          TabOrder = 2
          Text = '73'
        end
        object btnLucasRecur: TButton
          Left = 32
          Top = 56
          Width = 153
          Height = 21
          Caption = 'Lucas Recur'
          TabOrder = 3
          OnClick = btnLucasRecurClick
        end
        object mmoLucasRes: TMemo
          Left = 32
          Top = 88
          Width = 153
          Height = 305
          ScrollBars = ssVertical
          TabOrder = 4
          WordWrap = False
        end
        object mmoLucasMod: TMemo
          Left = 192
          Top = 88
          Width = 153
          Height = 305
          ScrollBars = ssVertical
          TabOrder = 5
          WordWrap = False
        end
        object btnLucasMod: TButton
          Left = 192
          Top = 56
          Width = 153
          Height = 21
          Caption = 'Lucas Mod'
          TabOrder = 6
          OnClick = btnLucasModClick
        end
      end
    end
  end
end
