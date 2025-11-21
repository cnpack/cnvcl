object FormOTS: TFormOTS
  Left = 192
  Top = 107
  Width = 1004
  Height = 578
  Caption = 'One Time Signature'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 971
    Height = 524
    ActivePage = tsSM3OTS
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsSM3OTS: TTabSheet
      Caption = 'SM3 OTS'
      object lblSM3OTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSM3OTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSM3OTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSM3OTSVerifyKey: TLabel
        Left = 674
        Top = 323
        Width = 55
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Verification:'
      end
      object lblSM3OTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSM3OTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SM3 OTS Keys'
        TabOrder = 0
        OnClick = btnGenSM3OTSKeysClick
      end
      object mmoSM3OTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSM3OTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSM3OTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SM3 OTS.')
        TabOrder = 3
      end
      object btnSM3OTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 OTS Sign'
        TabOrder = 4
        OnClick = btnSM3OTSSignClick
      end
      object btnSM3OTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 OTS Verify'
        TabOrder = 5
        OnClick = btnSM3OTSVerifyClick
      end
      object mmoSM3OTSVerificationKey: TMemo
        Left = 674
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
      object mmoSM3OTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
        WordWrap = False
      end
    end
    object tsSHA256OTS: TTabSheet
      Caption = 'SHA256 OTS'
      ImageIndex = 1
      object lblSHA256OTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSHA256OTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSHA256OTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSHA256OTSVerifyKey: TLabel
        Left = 674
        Top = 323
        Width = 55
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Verification:'
      end
      object lblSHA256OTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSHA256OTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SHA256 OTS Keys'
        TabOrder = 0
        OnClick = btnGenSHA256OTSKeysClick
      end
      object mmoSHA256OTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSHA256OTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSHA256OTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SHA256 OTS.')
        TabOrder = 3
      end
      object btnSHA256OTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 OTS Sign'
        TabOrder = 4
        OnClick = btnSHA256OTSSignClick
      end
      object btnSHA256OTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 OTS Verify'
        TabOrder = 5
        OnClick = btnSHA256OTSVerifyClick
      end
      object mmoSHA256OTSVerificationKey: TMemo
        Left = 674
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
      object mmoSHA256OTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
        WordWrap = False
      end
    end
    object tsSM3MOTS: TTabSheet
      Caption = 'SM3 M-OTS'
      ImageIndex = 2
      object lblSM3MOTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSM3MOTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSM3MOTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSM3MOTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSM3MOTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SM3 MOTS Keys'
        TabOrder = 0
        OnClick = btnGenSM3MOTSKeysClick
      end
      object mmoSM3MOTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSM3MOTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSM3MOTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SM3 MOTS.')
        TabOrder = 3
      end
      object btnSM3MOTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 MOTS Sign'
        TabOrder = 4
        OnClick = btnSM3MOTSSignClick
      end
      object btnSM3MOTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 MOTS Verify'
        TabOrder = 5
        OnClick = btnSM3MOTSVerifyClick
      end
      object mmoSM3MOTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 535
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
    end
    object tsSHA256MOTS: TTabSheet
      Caption = 'SHA256 M-OTS'
      ImageIndex = 3
      object lblSHA256MOTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSHA256MOTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSHA256MOTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSHA256MOTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSHA256MOTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SHA256 MOTS Keys'
        TabOrder = 0
        OnClick = btnGenSHA256MOTSKeysClick
      end
      object mmoSHA256MOTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSHA256MOTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSHA256MOTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SHA256 MOTS.')
        TabOrder = 3
      end
      object btnSHA256MOTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 MOTS Sign'
        TabOrder = 4
        OnClick = btnSHA256MOTSSignClick
      end
      object btnSHA256MOTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 MOTS Verify'
        TabOrder = 5
        OnClick = btnSHA256MOTSVerifyClick
      end
      object mmoSHA256MOTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 535
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
    end
    object tsSM3WOTS: TTabSheet
      Caption = 'SM3 W-OTS'
      ImageIndex = 4
      object lblSM3WOTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSM3WOTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSM3WOTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSM3WOTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSM3WOTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SM3 WOTS Keys'
        TabOrder = 0
        OnClick = btnGenSM3WOTSKeysClick
      end
      object mmoSM3WOTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSM3WOTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSM3WOTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SM3 WOTS.')
        TabOrder = 3
      end
      object btnSM3WOTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 WOTS Sign'
        TabOrder = 4
        OnClick = btnSM3WOTSSignClick
      end
      object btnSM3WOTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 WOTS Verify'
        TabOrder = 5
        OnClick = btnSM3WOTSVerifyClick
      end
      object mmoSM3WOTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 535
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
    end
    object tsSHA256WOTS: TTabSheet
      Caption = 'SHA256 W-OTS'
      ImageIndex = 5
      object lblSHA256WOTSMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSHA256WOTSPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSHA256WOTSPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSHA256WOTSSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object btnGenSHA256WOTSKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SHA256 WOTS Keys'
        TabOrder = 0
        OnClick = btnGenSHA256WOTSKeysClick
      end
      object mmoSHA256WOTSPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSHA256WOTSPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSHA256WOTSMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SHA256 WOTS.')
        TabOrder = 3
      end
      object btnSHA256WOTSSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 WOTS Sign'
        TabOrder = 4
        OnClick = btnSHA256WOTSSignClick
      end
      object btnSHA256WOTSVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 WOTS Verify'
        TabOrder = 5
        OnClick = btnSHA256WOTSVerifyClick
      end
      object mmoSHA256WOTSSignature: TMemo
        Left = 402
        Top = 347
        Width = 535
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
    end
    object tsSM3WOTSPlus: TTabSheet
      Caption = 'SM3 W-OTS+'
      ImageIndex = 6
      object lblSM3WOTSPlusMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSM3WOTSPlusPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSM3WOTSPlusPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSM3WOTSPlusSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object lblSM3WOTSPlusSalt: TLabel
        Left = 674
        Top = 323
        Width = 29
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Mask:'
      end
      object btnGenSM3WOTSPlusKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SM3 WOTS+ Keys'
        TabOrder = 0
        OnClick = btnGenSM3WOTSPlusKeysClick
      end
      object mmoSM3WOTSPlusPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSM3WOTSPlusPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSM3WOTSPlusMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SM3 WOTS+.')
        TabOrder = 3
      end
      object btnSM3WOTSPlusSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 WOTS+ Sign'
        TabOrder = 4
        OnClick = btnSM3WOTSPlusSignClick
      end
      object btnSM3WOTSPlusVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SM3 WOTS+ Verify'
        TabOrder = 5
        OnClick = btnSM3WOTSPlusVerifyClick
      end
      object mmoSM3WOTSPlusSignature: TMemo
        Left = 402
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
      object mmoSM3WOTSPlusMask: TMemo
        Left = 674
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
        WordWrap = False
      end
    end
    object tsSHA256WOTSPlus: TTabSheet
      Caption = 'SHA256 W-OTS+'
      ImageIndex = 7
      object lblSHA256WOTSPlusMessage: TLabel
        Left = 8
        Top = 323
        Width = 46
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Message:'
      end
      object lblSHA256WOTSPlusPrivate: TLabel
        Left = 8
        Top = 40
        Width = 36
        Height = 13
        Caption = 'Private:'
      end
      object lblSHA256WOTSPlusPublic: TLabel
        Left = 8
        Top = 176
        Width = 32
        Height = 13
        Caption = 'Public:'
      end
      object lblSHA256WOTSPlusSignature: TLabel
        Left = 402
        Top = 323
        Width = 48
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Signature:'
      end
      object lblSHA256WOTSPlusMask: TLabel
        Left = 674
        Top = 323
        Width = 29
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Mask:'
      end
      object btnGenSHA256WOTSPlusKeys: TButton
        Left = 8
        Top = 8
        Width = 177
        Height = 25
        Caption = 'Generate SHA256 WOTS+ Keys'
        TabOrder = 0
        OnClick = btnGenSHA256WOTSPlusKeysClick
      end
      object mmoSHA256WOTSPlusPrivateKey: TMemo
        Left = 80
        Top = 40
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object mmoSHA256WOTSPlusPublicKey: TMemo
        Left = 80
        Top = 176
        Width = 859
        Height = 129
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssVertical
        TabOrder = 2
        WordWrap = False
      end
      object mmoSHA256WOTSPlusMessage: TMemo
        Left = 80
        Top = 323
        Width = 257
        Height = 81
        Anchors = [akLeft, akBottom]
        Lines.Strings = (
          'Test message for SHA256 WOTS+.')
        TabOrder = 3
      end
      object btnSHA256WOTSPlusSign: TButton
        Left = 80
        Top = 419
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 WOTS+ Sign'
        TabOrder = 4
        OnClick = btnSHA256WOTSPlusSignClick
      end
      object btnSHA256WOTSPlusVerify: TButton
        Left = 80
        Top = 451
        Width = 257
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'SHA256 WOTS+ Verify'
        TabOrder = 5
        OnClick = btnSHA256WOTSPlusVerifyClick
      end
      object mmoSHA256WOTSPlusSignature: TMemo
        Left = 402
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 6
        WordWrap = False
      end
      object mmoSHA256WOTSPlusMask: TMemo
        Left = 674
        Top = 347
        Width = 265
        Height = 128
        Anchors = [akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
        WordWrap = False
      end
    end
  end
end
