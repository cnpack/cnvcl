object FormPaillier: TFormPaillier
  Left = 192
  Top = 108
  Width = 979
  Height = 608
  Caption = 'Test Paillier'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object pgcPaillier: TPageControl
    Left = 8
    Top = 8
    Width = 953
    Height = 562
    ActivePage = tsInt64Paillier
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsInt64Paillier: TTabSheet
      Caption = 'Int64 Paillier'
      object grpInt64Paillier: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 517
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Int64 Paillier'
        TabOrder = 0
        object lblInt64Public: TLabel
          Left = 216
          Top = 28
          Width = 54
          Height = 12
          Caption = 'Public N:'
        end
        object lblInt64PublicG: TLabel
          Left = 416
          Top = 28
          Width = 12
          Height = 12
          Caption = 'G:'
        end
        object lblInt64PrivateP: TLabel
          Left = 216
          Top = 60
          Width = 60
          Height = 12
          Caption = 'Private P:'
        end
        object lblInt64PrivateQ: TLabel
          Left = 416
          Top = 60
          Width = 12
          Height = 12
          Caption = 'Q:'
        end
        object lblInt64PrivateLambda: TLabel
          Left = 576
          Top = 60
          Width = 42
          Height = 12
          Caption = 'Lambda:'
        end
        object lblInt64PrivateMu: TLabel
          Left = 760
          Top = 60
          Width = 18
          Height = 12
          Caption = 'Mu:'
        end
        object lblInt64Data: TLabel
          Left = 216
          Top = 92
          Width = 30
          Height = 12
          Caption = 'Data:'
        end
        object bvl1: TBevel
          Left = 576
          Top = 24
          Width = 9
          Height = 25
          Shape = bsLeftLine
        end
        object lblInt64PublicN2: TLabel
          Left = 592
          Top = 28
          Width = 24
          Height = 12
          Caption = 'N^2:'
        end
        object bvl2: TBevel
          Left = 16
          Top = 120
          Width = 897
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object lblInt64Data1: TLabel
          Left = 16
          Top = 148
          Width = 36
          Height = 12
          Caption = '明文1:'
        end
        object lblInt64Data2: TLabel
          Left = 280
          Top = 148
          Width = 48
          Height = 12
          Caption = '+ 明文2:'
        end
        object lblInt64Data3: TLabel
          Left = 584
          Top = 148
          Width = 54
          Height = 12
          Caption = '=  明文3:'
        end
        object lblInt64Enc1: TLabel
          Left = 16
          Top = 188
          Width = 36
          Height = 12
          Caption = '密文1:'
        end
        object lblInt64Enc2: TLabel
          Left = 280
          Top = 188
          Width = 48
          Height = 12
          Caption = '* 密文2:'
        end
        object lblInt64Enc3: TLabel
          Left = 584
          Top = 188
          Width = 54
          Height = 12
          Caption = '=  密文3:'
        end
        object btnInt64PaillierSample: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample'
          TabOrder = 0
          OnClick = btnInt64PaillierSampleClick
        end
        object btnGenerateKey: TButton
          Left = 120
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Gen Key'
          TabOrder = 1
          OnClick = btnGenerateKeyClick
        end
        object edtInt64PublicN: TEdit
          Left = 272
          Top = 24
          Width = 121
          Height = 20
          TabOrder = 2
          Text = '3830097319'
          OnChange = edtInt64PublicNChange
        end
        object edtInt64PublicG: TEdit
          Left = 440
          Top = 24
          Width = 121
          Height = 20
          TabOrder = 3
          Text = '3830097320'
        end
        object edtInt64PrivateP: TEdit
          Left = 272
          Top = 56
          Width = 121
          Height = 20
          TabOrder = 4
          Text = '61723'
        end
        object edtInt64PrivateQ: TEdit
          Left = 440
          Top = 56
          Width = 121
          Height = 20
          TabOrder = 5
          Text = '62053'
        end
        object edtInt64PrivateLambda: TEdit
          Left = 624
          Top = 56
          Width = 121
          Height = 20
          TabOrder = 6
          Text = '638328924'
        end
        object edtInt64PrivateMu: TEdit
          Left = 792
          Top = 56
          Width = 121
          Height = 20
          TabOrder = 7
          Text = '1352223169'
        end
        object edtInt64Data: TEdit
          Left = 272
          Top = 88
          Width = 289
          Height = 20
          TabOrder = 8
          Text = '23333'
        end
        object btnInt64Encrypt: TButton
          Left = 16
          Top = 56
          Width = 177
          Height = 25
          Caption = 'Encrypt/Decrypt'
          TabOrder = 9
          OnClick = btnInt64EncryptClick
        end
        object edtInt64PublicN2: TEdit
          Left = 624
          Top = 24
          Width = 289
          Height = 20
          TabOrder = 10
        end
        object edtInt64Data1: TEdit
          Left = 72
          Top = 144
          Width = 193
          Height = 20
          TabOrder = 11
          Text = '23'
        end
        object edtInt64Data2: TEdit
          Left = 336
          Top = 144
          Width = 217
          Height = 20
          TabOrder = 12
          Text = '74'
        end
        object edtInt64Data3: TEdit
          Left = 656
          Top = 144
          Width = 257
          Height = 20
          TabOrder = 13
        end
        object edtInt64Enc1: TEdit
          Left = 72
          Top = 184
          Width = 193
          Height = 20
          TabOrder = 14
        end
        object edtInt64Enc2: TEdit
          Left = 336
          Top = 184
          Width = 217
          Height = 20
          TabOrder = 15
        end
        object edtInt64Enc3: TEdit
          Left = 656
          Top = 184
          Width = 257
          Height = 20
          TabOrder = 16
        end
        object btnChecknt64AddHomo: TButton
          Left = 16
          Top = 224
          Width = 897
          Height = 25
          Caption = '验证加法同态'
          TabOrder = 17
          OnClick = btnChecknt64AddHomoClick
        end
        object btnInt64PaillierSample2: TButton
          Left = 16
          Top = 472
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Sample Check1'
          TabOrder = 18
          OnClick = btnInt64PaillierSample2Click
        end
        object btnInt64PaillierSample3: TButton
          Left = 136
          Top = 472
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Sample Check2'
          TabOrder = 19
          OnClick = btnInt64PaillierSample3Click
        end
        object btnInt64PaillierSample4: TButton
          Left = 264
          Top = 472
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Check3'
          TabOrder = 20
          OnClick = btnInt64PaillierSample4Click
        end
        object btnInt64PaillierSample5: TButton
          Left = 392
          Top = 472
          Width = 105
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Check4'
          TabOrder = 21
          OnClick = btnInt64PaillierSample5Click
        end
      end
    end
    object tsBigNumberPaillier: TTabSheet
      Caption = 'BigNumber Paillier'
      ImageIndex = 1
      object grpBNPaillier: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 517
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'BigNumber Paillier'
        TabOrder = 0
        object lblBNPublic: TLabel
          Left = 16
          Top = 68
          Width = 54
          Height = 12
          Caption = 'Public N:'
        end
        object lblBNPublicG: TLabel
          Left = 16
          Top = 100
          Width = 12
          Height = 12
          Caption = 'G:'
        end
        object lblBNPrivateP: TLabel
          Left = 16
          Top = 180
          Width = 60
          Height = 12
          Caption = 'Private P:'
        end
        object lblBNPrivateQ: TLabel
          Left = 48
          Top = 212
          Width = 12
          Height = 12
          Caption = 'Q:'
        end
        object lblBNPrivateLambda: TLabel
          Left = 16
          Top = 244
          Width = 42
          Height = 12
          Caption = 'Lambda:'
        end
        object lblBNPrivateMu: TLabel
          Left = 40
          Top = 276
          Width = 18
          Height = 12
          Caption = 'Mu:'
        end
        object lblBNData: TLabel
          Left = 424
          Top = 28
          Width = 30
          Height = 12
          Caption = 'Data:'
        end
        object bvl11: TBevel
          Left = 16
          Top = 160
          Width = 897
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object lbBNPublicN2: TLabel
          Left = 16
          Top = 132
          Width = 24
          Height = 12
          Caption = 'N^2:'
        end
        object bvl111: TBevel
          Left = 16
          Top = 384
          Width = 897
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Shape = bsTopLine
        end
        object lblBNData1: TLabel
          Left = 16
          Top = 404
          Width = 36
          Height = 12
          Caption = '明文1:'
        end
        object lblBNEnc1: TLabel
          Left = 16
          Top = 444
          Width = 36
          Height = 12
          Caption = '密文1:'
        end
        object lblBNEnc2: TLabel
          Left = 280
          Top = 444
          Width = 48
          Height = 12
          Caption = '* 密文2:'
        end
        object lblBNData2: TLabel
          Left = 280
          Top = 404
          Width = 48
          Height = 12
          Caption = '+ 明文2:'
        end
        object lblBNData3: TLabel
          Left = 584
          Top = 404
          Width = 54
          Height = 12
          Caption = '=  明文3:'
        end
        object lblBNEnc3: TLabel
          Left = 584
          Top = 444
          Width = 54
          Height = 12
          Caption = '=  密文3:'
        end
        object btnBNPaillierSample: TButton
          Left = 16
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Sample'
          TabOrder = 0
          OnClick = btnBNPaillierSampleClick
        end
        object btnBNGenerateKey: TButton
          Left = 120
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Gen Key'
          TabOrder = 1
          OnClick = btnBNGenerateKeyClick
        end
        object edtBNPublicN: TEdit
          Left = 72
          Top = 64
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = '3830097319'
          OnChange = edtBNPublicNChange
        end
        object edtBNPublicG: TEdit
          Left = 72
          Top = 96
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          Text = '3830097320'
        end
        object edtBNPrivateP: TEdit
          Left = 72
          Top = 176
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          Text = '61723'
        end
        object edtBNPrivateQ: TEdit
          Left = 72
          Top = 208
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          Text = '62053'
        end
        object edtBNPrivateLambda: TEdit
          Left = 72
          Top = 240
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
          Text = '638328924'
        end
        object edtBNPrivateMu: TEdit
          Left = 72
          Top = 272
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
          Text = '1352223169'
        end
        object edtBNData: TEdit
          Left = 456
          Top = 24
          Width = 457
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 8
          Text = '23333'
        end
        object btnBNEncrypt: TButton
          Left = 232
          Top = 24
          Width = 177
          Height = 25
          Caption = 'Encrypt/Decrypt'
          TabOrder = 9
          OnClick = btnBNEncryptClick
        end
        object edtBNPublicN2: TEdit
          Left = 72
          Top = 128
          Width = 841
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 10
        end
        object mmoBNResult: TMemo
          Left = 72
          Top = 304
          Width = 841
          Height = 65
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 11
        end
        object edtBNData1: TEdit
          Left = 72
          Top = 400
          Width = 193
          Height = 20
          TabOrder = 12
          Text = '6543209873'
        end
        object edtBNEnc1: TEdit
          Left = 72
          Top = 440
          Width = 193
          Height = 20
          TabOrder = 13
        end
        object edtBNData2: TEdit
          Left = 336
          Top = 400
          Width = 217
          Height = 20
          TabOrder = 14
          Text = '744567897654321396234'
        end
        object edtBNEnc2: TEdit
          Left = 336
          Top = 440
          Width = 217
          Height = 20
          TabOrder = 15
        end
        object edtBNEnc3: TEdit
          Left = 656
          Top = 440
          Width = 257
          Height = 20
          TabOrder = 16
        end
        object edtBNData3: TEdit
          Left = 656
          Top = 400
          Width = 257
          Height = 20
          TabOrder = 17
        end
        object btnCheckBNAddHomo: TButton
          Left = 16
          Top = 480
          Width = 897
          Height = 25
          Caption = '验证加法同态'
          TabOrder = 18
          OnClick = btnCheckBNAddHomoClick
        end
      end
    end
  end
end
