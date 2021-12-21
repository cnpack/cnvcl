object FormEcc: TFormEcc
  Left = 346
  Top = 146
  Width = 860
  Height = 598
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
    Width = 820
    Height = 535
    ActivePage = tsSimpleECC
    Anchors = [akLeft, akTop, akRight, akBottom]
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
        Width = 780
        Height = 161
        Anchors = [akLeft, akTop, akRight]
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
          Left = 103
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 0+P'
          TabOrder = 1
          OnClick = btnTest0Click
        end
        object btnTestOn: TButton
          Left = 186
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test On Curve'
          TabOrder = 2
          OnClick = btnTestOnClick
        end
        object btnTestInverse: TButton
          Left = 270
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test Inverse'
          TabOrder = 3
          OnClick = btnTestInverseClick
        end
        object btnTest2P: TButton
          Left = 353
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test 2*P'
          TabOrder = 4
          OnClick = btnTest2PClick
        end
        object btnTestMul: TButton
          Left = 436
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
        object btnInt64Affine: TButton
          Left = 520
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test Affine +'
          TabOrder = 15
          OnClick = btnInt64AffineClick
        end
        object btnTestJacobian: TButton
          Left = 600
          Top = 28
          Width = 75
          Height = 21
          Caption = 'Test Jacobian +'
          TabOrder = 16
          OnClick = btnTestJacobianClick
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
        Width = 780
        Height = 471
        Anchors = [akLeft, akTop, akRight, akBottom]
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
          Left = 651
          Top = 24
          Width = 106
          Height = 21
          Anchors = [akTop, akRight]
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
          Left = 651
          Top = 56
          Width = 106
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Calculate N*G'
          TabOrder = 7
          OnClick = btnCalcNGClick
        end
        object mmoGenECCPoints: TMemo
          Left = 568
          Top = 96
          Width = 188
          Height = 359
          Anchors = [akLeft, akTop, akRight, akBottom]
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
          Left = 483
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Legendre'
          TabOrder = 9
          OnClick = btnLeRanDeClick
        end
        object btnBNGXtoPoint: TButton
          Left = 483
          Top = 434
          Width = 75
          Height = 21
          Anchors = [akLeft, akBottom]
          Caption = 'BN G.X to Pt'
          TabOrder = 12
          OnClick = btnBNGXtoPointClick
        end
        object btnInt64GXtoPt: TButton
          Left = 483
          Top = 128
          Width = 75
          Height = 21
          Caption = 'G.X to Pt'
          TabOrder = 11
          OnClick = btnInt64GXtoPtClick
        end
        object cbbInt64EccPreset: TComboBox
          Left = 568
          Top = 24
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 13
          OnChange = cbbInt64EccPresetChange
          Items.Strings = (
            'Preset 1'
            'Preset 2')
        end
        object btnEccTestAdd: TButton
          Left = 483
          Top = 156
          Width = 75
          Height = 21
          Caption = '? * G'
          TabOrder = 14
          OnClick = btnEccTestAddClick
        end
        object btnHassenTest: TButton
          Left = 483
          Top = 200
          Width = 75
          Height = 25
          Caption = 'Hasse Test 1'
          TabOrder = 15
          OnClick = btnHassenTestClick
        end
        object btnHassenTest2: TButton
          Left = 483
          Top = 232
          Width = 75
          Height = 25
          Caption = 'Hasse Test 2'
          TabOrder = 16
          OnClick = btnHassenTest2Click
        end
        object btnInt64SchoofTest: TButton
          Left = 482
          Top = 296
          Width = 75
          Height = 21
          Caption = 'Schoof Test'
          TabOrder = 17
          OnClick = btnInt64SchoofTestClick
        end
        object btnInt64EccCountOrder: TButton
          Left = 481
          Top = 328
          Width = 75
          Height = 21
          Caption = 'Count Order1'
          TabOrder = 18
          OnClick = btnInt64EccCountOrderClick
        end
        object btnInt64CountOrder1: TButton
          Left = 481
          Top = 360
          Width = 75
          Height = 21
          Caption = 'Count Order2'
          TabOrder = 19
          OnClick = btnInt64CountOrder1Click
        end
        object btnInt64CountEccPoints3: TButton
          Left = 481
          Top = 392
          Width = 75
          Height = 21
          Caption = 'Count Order3'
          TabOrder = 20
          OnClick = btnInt64CountEccPoints3Click
        end
      end
    end
    object tsECC: TTabSheet
      Caption = 'BigNumber ECC 1'
      ImageIndex = 2
      object grpBNEcc: TGroupBox
        Left = 16
        Top = 16
        Width = 780
        Height = 479
        Anchors = [akLeft, akTop, akRight, akBottom]
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
          Width = 745
          Height = 17
          Anchors = [akLeft, akTop, akRight]
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
          Width = 660
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = '0'
        end
        object edtBNEccB: TEdit
          Left = 56
          Top = 56
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = '7'
        end
        object edtBNEccP: TEdit
          Left = 56
          Top = 88
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
        object edtBNEccGX: TEdit
          Left = 56
          Top = 120
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
        end
        object edtBNEccGY: TEdit
          Left = 56
          Top = 152
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
        end
        object edtBNEccOrder: TEdit
          Left = 56
          Top = 184
          Width = 665
          Height = 21
          Anchors = [akLeft, akTop, akRight]
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
          Width = 748
          Height = 21
          Anchors = [akLeft, akTop, akRight]
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
          Width = 748
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 17
        end
        object edtBNEccPrivateKey: TEdit
          Left = 232
          Top = 300
          Width = 532
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 16
        end
        object edtBNEccDataPoint: TEdit
          Left = 80
          Top = 372
          Width = 588
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 18
        end
        object btnBNEccCrypt: TButton
          Left = 675
          Top = 370
          Width = 91
          Height = 21
          Anchors = [akRight]
          Caption = 'Encrypt/Decrypt'
          TabOrder = 19
          OnClick = btnBNEccCryptClick
        end
        object btnBNUpdate: TButton
          Left = 728
          Top = 184
          Width = 35
          Height = 21
          Caption = 'Set'
          TabOrder = 20
          OnClick = btnBNUpdateClick
        end
        object btnBNEccCalc: TButton
          Left = 678
          Top = 264
          Width = 75
          Height = 21
          Caption = '? * G'
          TabOrder = 21
          OnClick = btnBNEccCalcClick
        end
      end
    end
    object tsWrapData: TTabSheet
      Caption = 'BigNumber ECC 2'
      ImageIndex = 3
      object grpWrap: TGroupBox
        Left = 16
        Top = 16
        Width = 780
        Height = 329
        Anchors = [akLeft, akTop, akRight, akBottom]
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
          Width = 532
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = '123321123456734234'
        end
        object btnWrapData: TButton
          Left = 595
          Top = 24
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'X to Point'
          TabOrder = 1
          OnClick = btnWrapDataClick
        end
        object edtWrapPoint: TEdit
          Left = 16
          Top = 56
          Width = 740
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
        end
        object edtBNECDHXa: TEdit
          Left = 70
          Top = 116
          Width = 598
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          Text = '30954823978126165439807650540507823796164384908578170843'
        end
        object edtBNECDHXb: TEdit
          Left = 70
          Top = 212
          Width = 598
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 9
          Text = '554978037890271980743250435089743764116321467834980754709'
        end
        object btnBNECDHYb: TButton
          Left = 683
          Top = 212
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Calc Yb'
          TabOrder = 10
          OnClick = btnBNECDHYbClick
        end
        object btnBNECDHYa: TButton
          Left = 683
          Top = 116
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Calc Ya'
          TabOrder = 5
          OnClick = btnBNECDHYaClick
        end
        object edtBNECDHA: TEdit
          Left = 70
          Top = 148
          Width = 598
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
        end
        object edtBNECDHB: TEdit
          Left = 70
          Top = 244
          Width = 598
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 11
        end
        object btnBNECDHBkey: TButton
          Left = 683
          Top = 244
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'B Calc Key'
          TabOrder = 12
          OnClick = btnBNECDHBkeyClick
        end
        object btnBNECDHAKey: TButton
          Left = 683
          Top = 148
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'A Calc Key'
          TabOrder = 7
          OnClick = btnBNECDHAKeyClick
        end
        object edtBNECDHResA: TEdit
          Left = 70
          Top = 180
          Width = 598
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 8
        end
        object edtBNECDHResB: TEdit
          Left = 70
          Top = 276
          Width = 598
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 13
        end
        object btnTestECDH: TButton
          Left = 683
          Top = 276
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'ECDH Sample'
          TabOrder = 14
          OnClick = btnTestECDHClick
        end
        object btnBNEccWrapRange: TButton
          Left = 683
          Top = 24
          Width = 75
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Range to Point'
          TabOrder = 2
          OnClick = btnBNEccWrapRangeClick
        end
      end
      object btnEccSchoof: TButton
        Left = 210
        Top = 368
        Width = 75
        Height = 21
        Caption = 'Schoof Test'
        TabOrder = 1
        OnClick = btnEccSchoofClick
      end
      object btnSimpleAttack: TButton
        Left = 16
        Top = 368
        Width = 75
        Height = 21
        Caption = 'Simple Attack'
        TabOrder = 2
        OnClick = btnSimpleAttackClick
      end
      object btnTestCRT: TButton
        Left = 112
        Top = 368
        Width = 75
        Height = 21
        Caption = 'CRT Sample'
        TabOrder = 3
        OnClick = btnTestCRTClick
      end
      object mmoBNEccPoints: TMemo
        Left = 304
        Top = 368
        Width = 217
        Height = 121
        TabOrder = 4
      end
    end
    object tsLucas: TTabSheet
      Caption = 'Lucas Sequence && Legendre'
      ImageIndex = 4
      object grpLucas: TGroupBox
        Left = 16
        Top = 16
        Width = 361
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
          Anchors = [akLeft, akTop, akBottom]
          ScrollBars = ssVertical
          TabOrder = 4
          WordWrap = False
        end
        object mmoLucasMod: TMemo
          Left = 192
          Top = 88
          Width = 153
          Height = 305
          Anchors = [akLeft, akTop, akBottom]
          ScrollBars = ssVertical
          TabOrder = 5
          WordWrap = False
        end
        object btnLucasMod: TButton
          Left = 192
          Top = 56
          Width = 65
          Height = 21
          Caption = 'Lucas 2'
          TabOrder = 6
          OnClick = btnLucasModClick
        end
        object chkLucasMod: TCheckBox
          Left = 296
          Top = 28
          Width = 49
          Height = 17
          Caption = 'Mod'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object btnBNLucasMod: TButton
          Left = 264
          Top = 56
          Width = 81
          Height = 21
          Caption = 'BN Lucas Mod'
          TabOrder = 8
          OnClick = btnBNLucasModClick
        end
      end
      object grpLegendre: TGroupBox
        Left = 392
        Top = 16
        Width = 404
        Height = 479
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Legendre Symbol'
        TabOrder = 1
        object btnCalcLegendre: TButton
          Left = 16
          Top = 24
          Width = 372
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Calc Legendre'
          TabOrder = 0
          OnClick = btnCalcLegendreClick
        end
        object mmoLegendre: TMemo
          Left = 16
          Top = 56
          Width = 193
          Height = 407
          Anchors = [akLeft, akTop, akBottom]
          ScrollBars = ssVertical
          TabOrder = 1
          WordWrap = False
        end
        object mmoLegendreRes1: TMemo
          Left = 221
          Top = 56
          Width = 52
          Height = 407
          Anchors = [akLeft, akTop, akBottom]
          ScrollBars = ssVertical
          TabOrder = 2
          WordWrap = False
        end
        object mmoLegendreRes2: TMemo
          Left = 280
          Top = 56
          Width = 57
          Height = 407
          ScrollBars = ssVertical
          TabOrder = 3
          WordWrap = False
        end
      end
    end
    object tsTonelliShanks: TTabSheet
      Caption = 'Tonelli Shanks'
      ImageIndex = 5
      object grpTonelliShanks: TGroupBox
        Left = 16
        Top = 16
        Width = 780
        Height = 455
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Tonelli Shanks'
        TabOrder = 0
        object lblTSX: TLabel
          Left = 16
          Top = 28
          Width = 10
          Height = 13
          Caption = 'X:'
        end
        object lblTSP: TLabel
          Left = 16
          Top = 60
          Width = 10
          Height = 13
          Caption = 'P:'
        end
        object edtTSX: TEdit
          Left = 40
          Top = 24
          Width = 588
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = '5'
        end
        object edtTSP: TEdit
          Left = 40
          Top = 56
          Width = 588
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = '538879667'
        end
        object btnTSInt64: TButton
          Left = 635
          Top = 24
          Width = 131
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Tonelli Shanks Int64'
          TabOrder = 2
          OnClick = btnTSInt64Click
        end
        object btnBNTS: TButton
          Left = 635
          Top = 56
          Width = 131
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Tonelli Shanks BigNumber'
          TabOrder = 3
          OnClick = btnBNTSClick
        end
        object mmoTSData: TMemo
          Left = 40
          Top = 120
          Width = 185
          Height = 241
          ScrollBars = ssVertical
          TabOrder = 4
          WordWrap = False
        end
        object btnRandomTS: TButton
          Left = 40
          Top = 88
          Width = 369
          Height = 21
          Caption = 'Random Tonelli Shanks using Int64 and BigNumber to 538879667'
          TabOrder = 5
        end
      end
    end
    object tsSquareRoot: TTabSheet
      Caption = 'Square Root'
      ImageIndex = 6
      object grpSquareRoot: TGroupBox
        Left = 8
        Top = 16
        Width = 788
        Height = 479
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Square Root'
        TabOrder = 0
        object lblSRY: TLabel
          Left = 16
          Top = 28
          Width = 19
          Height = 13
          Caption = 'Y^2'
        end
        object lblSREqual: TLabel
          Left = 184
          Top = 28
          Width = 6
          Height = 13
          Caption = '='
        end
        object lblSRMod: TLabel
          Left = 336
          Top = 28
          Width = 30
          Height = 13
          Caption = 'mod P'
        end
        object edtSRY: TEdit
          Left = 48
          Top = 24
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object edtSRX: TEdit
          Left = 200
          Top = 24
          Width = 121
          Height = 21
          TabOrder = 1
          Text = '55'
        end
        object edtSRP: TEdit
          Left = 376
          Top = 24
          Width = 121
          Height = 21
          TabOrder = 2
          Text = '103'
        end
        object btnSRLucas: TButton
          Left = 520
          Top = 24
          Width = 121
          Height = 21
          Caption = 'Calc Y and Check'
          TabOrder = 3
          OnClick = btnSRLucasClick
        end
        object btnSRCompare: TButton
          Left = 48
          Top = 64
          Width = 449
          Height = 25
          Caption = 
            'Compare Lucas Sequence and Toneli-Shanks for Int64 when P = 8*u ' +
            '+ 1'
          TabOrder = 4
          OnClick = btnSRCompareClick
        end
      end
    end
    object tsPem: TTabSheet
      Caption = 'Keys Load/Save and Sig / Verify'
      ImageIndex = 7
      object grpEccKeys: TGroupBox
        Left = 16
        Top = 16
        Width = 777
        Height = 473
        Caption = 'Ecc Keys'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object lblCurveType: TLabel
          Left = 112
          Top = 28
          Width = 58
          Height = 13
          Caption = 'Curve Type:'
        end
        object lblCurveTypeText: TLabel
          Left = 184
          Top = 28
          Width = 46
          Height = 13
          Caption = 'Unknown'
        end
        object Label1: TLabel
          Left = 16
          Top = 228
          Width = 29
          Height = 13
          Caption = 'Order:'
        end
        object Label2: TLabel
          Left = 16
          Top = 196
          Width = 18
          Height = 13
          Caption = 'GY:'
        end
        object Label3: TLabel
          Left = 16
          Top = 164
          Width = 18
          Height = 13
          Caption = 'GX:'
        end
        object Label4: TLabel
          Left = 16
          Top = 132
          Width = 20
          Height = 13
          Caption = 'mod'
        end
        object Label5: TLabel
          Left = 16
          Top = 100
          Width = 19
          Height = 13
          Caption = 'X + '
        end
        object Label6: TLabel
          Left = 16
          Top = 68
          Width = 92
          Height = 13
          Caption = 'Y ^ 2 = X ^ 3    +    '
        end
        object bvlKey: TBevel
          Left = 16
          Top = 256
          Width = 745
          Height = 17
          Shape = bsTopLine
        end
        object lblKeyPrivate: TLabel
          Left = 16
          Top = 276
          Width = 73
          Height = 13
          Caption = 'Private (Count):'
        end
        object lblKeyPublic: TLabel
          Left = 16
          Top = 308
          Width = 65
          Height = 13
          Caption = 'Public (Point):'
        end
        object bvlSig: TBevel
          Left = 16
          Top = 336
          Width = 745
          Height = 17
          Shape = bsTopLine
        end
        object lblKeyData: TLabel
          Left = 16
          Top = 356
          Width = 26
          Height = 13
          Caption = 'Data:'
        end
        object lblKeySign: TLabel
          Left = 16
          Top = 422
          Width = 18
          Height = 13
          Caption = 'Sig:'
        end
        object lblKeyHash: TLabel
          Left = 232
          Top = 390
          Width = 46
          Height = 13
          Caption = 'KeyHash:'
        end
        object btnLoad: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Load Key'
          TabOrder = 0
          OnClick = btnLoadClick
        end
        object edtKeyEccA: TEdit
          Left = 104
          Top = 64
          Width = 660
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
        object edtKeyEccB: TEdit
          Left = 56
          Top = 96
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
        object edtKeyEccP: TEdit
          Left = 56
          Top = 128
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
        end
        object edtKeyEccGX: TEdit
          Left = 56
          Top = 160
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
        end
        object edtKeyEccGY: TEdit
          Left = 56
          Top = 192
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
        end
        object edtKeyEccOrder: TEdit
          Left = 56
          Top = 224
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
        end
        object edtKeyPrivate: TEdit
          Left = 104
          Top = 272
          Width = 660
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
        end
        object edtKeyPublic: TEdit
          Left = 104
          Top = 304
          Width = 660
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 8
        end
        object btnKeyCheckPublic: TButton
          Left = 512
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Check Public'
          TabOrder = 9
          OnClick = btnKeyCheckPublicClick
        end
        object btnSaveKey: TButton
          Left = 688
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Save Key'
          TabOrder = 10
          OnClick = btnSaveKeyClick
        end
        object edtKeyData: TEdit
          Left = 56
          Top = 352
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 11
          Text = '1234567890ABCDEF'
        end
        object btnKeySign: TButton
          Left = 56
          Top = 384
          Width = 129
          Height = 25
          Caption = 'Sign Using PrivateKey'
          TabOrder = 12
          OnClick = btnKeySignClick
        end
        object edtKeySign: TEdit
          Left = 56
          Top = 418
          Width = 708
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 13
        end
        object btnKeyVerify: TButton
          Left = 634
          Top = 384
          Width = 129
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Verfiy Using PublicKey'
          TabOrder = 14
          OnClick = btnKeyVerifyClick
        end
        object cbbKeyHash: TComboBox
          Left = 288
          Top = 384
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 15
          Items.Strings = (
            'MD5'
            'SHA1'
            'SHA256'
            'SM3')
        end
        object btnKeyGenerate: TButton
          Left = 600
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Key Generate'
          TabOrder = 16
          OnClick = btnKeyGenerateClick
        end
        object btnKeyLoadSig: TButton
          Left = 488
          Top = 384
          Width = 121
          Height = 25
          Caption = 'Load Signature to Verify'
          TabOrder = 17
          OnClick = btnKeyLoadSigClick
        end
        object cbbCurveTypes: TComboBox
          Left = 336
          Top = 24
          Width = 161
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 18
          OnChange = cbbCurveTypesChange
        end
      end
    end
  end
  object dlgOpen1: TOpenDialog
    Left = 364
    Top = 248
  end
  object dlgSave1: TSaveDialog
    Left = 468
    Top = 152
  end
end
