object FormCA: TFormCA
  Left = 360
  Top = 146
  Width = 889
  Height = 613
  Caption = 'Certificate Authority'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 865
    Height = 567
    ActivePage = tsRequest
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsRequest: TTabSheet
      Caption = 'Certificate Sign Request'
      object grpGenRequest: TGroupBox
        Left = 8
        Top = 8
        Width = 841
        Height = 185
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Generate Certificate Sign Request'
        TabOrder = 0
        object lblKey: TLabel
          Left = 16
          Top = 24
          Width = 72
          Height = 13
          Caption = 'RSA/ECC Key:'
        end
        object lblContryName: TLabel
          Left = 16
          Top = 52
          Width = 64
          Height = 13
          Caption = 'Contry Name:'
        end
        object lblStateOrProvinceName: TLabel
          Left = 160
          Top = 52
          Width = 116
          Height = 13
          Caption = 'State or Province Name:'
        end
        object lblLocalityName: TLabel
          Left = 432
          Top = 52
          Width = 70
          Height = 13
          Caption = 'Locality Name:'
        end
        object lblOrgName: TLabel
          Left = 16
          Top = 84
          Width = 93
          Height = 13
          Caption = 'Organization Name:'
        end
        object lblOrgUnitName: TLabel
          Left = 304
          Top = 84
          Width = 123
          Height = 13
          Caption = 'Organizational Unit Name:'
        end
        object lblCommonName: TLabel
          Left = 16
          Top = 116
          Width = 75
          Height = 13
          Caption = 'Common Name:'
        end
        object lblEmail: TLabel
          Left = 304
          Top = 116
          Width = 69
          Height = 13
          Caption = 'Email Address:'
        end
        object lblHash: TLabel
          Left = 16
          Top = 148
          Width = 74
          Height = 13
          Caption = 'Hash Algorithm:'
        end
        object edtRSAECCKey: TEdit
          Left = 88
          Top = 20
          Width = 641
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btnBrowseKey: TButton
          Left = 736
          Top = 20
          Width = 91
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse Key File'
          TabOrder = 1
          OnClick = btnBrowseKeyClick
        end
        object edtContryName: TEdit
          Left = 88
          Top = 48
          Width = 49
          Height = 21
          TabOrder = 2
          Text = 'CN'
        end
        object edtStateOrProvinceName: TEdit
          Left = 288
          Top = 48
          Width = 121
          Height = 21
          TabOrder = 3
          Text = 'ZheJiang'
        end
        object edtLocalityName: TEdit
          Left = 512
          Top = 48
          Width = 129
          Height = 21
          TabOrder = 4
          Text = 'HangZhou'
        end
        object edtOrgName: TEdit
          Left = 120
          Top = 80
          Width = 161
          Height = 21
          TabOrder = 5
          Text = 'CnPack'
        end
        object edtOrgUnitName: TEdit
          Left = 440
          Top = 80
          Width = 201
          Height = 21
          TabOrder = 6
          Text = 'CnPack Team'
        end
        object edtCommonName: TEdit
          Left = 120
          Top = 112
          Width = 161
          Height = 21
          TabOrder = 7
          Text = 'cnpack.org'
        end
        object edtEmail: TEdit
          Left = 440
          Top = 112
          Width = 201
          Height = 21
          TabOrder = 8
          Text = 'master@cnpack.org'
        end
        object cbbHash: TComboBox
          Left = 120
          Top = 144
          Width = 161
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 9
          Items.Strings = (
            'MD5'
            'SHA1'
            'SHA256')
        end
        object btnGenerateCSR: TButton
          Left = 488
          Top = 144
          Width = 209
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Generate Certificate Sign Request'
          TabOrder = 10
          OnClick = btnGenerateCSRClick
        end
        object btnSelfSign: TButton
          Left = 712
          Top = 144
          Width = 113
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Self Sign'
          TabOrder = 11
          OnClick = btnSelfSignClick
        end
      end
      object grpParse: TGroupBox
        Left = 8
        Top = 200
        Width = 841
        Height = 327
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Parse CSR File'
        TabOrder = 1
        object lblCSR: TLabel
          Left = 16
          Top = 24
          Width = 44
          Height = 13
          Caption = 'CSR File:'
        end
        object edtCSR: TEdit
          Left = 88
          Top = 20
          Width = 497
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btnBrowseCSR: TButton
          Left = 592
          Top = 20
          Width = 73
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse CSR'
          TabOrder = 1
          OnClick = btnBrowseCSRClick
        end
        object mmoCSRParse: TMemo
          Left = 16
          Top = 56
          Width = 809
          Height = 255
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 4
        end
        object btnParseCSR: TButton
          Left = 672
          Top = 20
          Width = 73
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Parse CSR'
          TabOrder = 2
          OnClick = btnParseCSRClick
        end
        object btnVerifyCSR: TButton
          Left = 752
          Top = 20
          Width = 73
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Verify CSR'
          TabOrder = 3
          OnClick = btnVerifyCSRClick
        end
      end
    end
    object tsSign: TTabSheet
      Caption = 'Sign Certificate'
      ImageIndex = 1
      object grpSign: TGroupBox
        Left = 8
        Top = 8
        Width = 841
        Height = 153
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Sign CSR File'
        TabOrder = 0
        object lblSignCSR: TLabel
          Left = 16
          Top = 24
          Width = 44
          Height = 13
          Caption = 'CSR File:'
        end
        object lblRoot: TLabel
          Left = 16
          Top = 56
          Width = 47
          Height = 13
          Caption = 'Root Key:'
        end
        object lblRootCrt: TLabel
          Left = 16
          Top = 88
          Width = 51
          Height = 13
          Caption = 'Root CRT:'
        end
        object edtSignCSR: TEdit
          Left = 88
          Top = 20
          Width = 641
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btnSignCSRBrowse: TButton
          Left = 736
          Top = 20
          Width = 91
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse CSR File'
          TabOrder = 1
          OnClick = btnSignCSRBrowseClick
        end
        object edtSignKey: TEdit
          Left = 88
          Top = 52
          Width = 641
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
        object btnSignKeyBrowse: TButton
          Left = 736
          Top = 52
          Width = 91
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse Key File'
          TabOrder = 3
          OnClick = btnSignKeyBrowseClick
        end
        object btnSign: TButton
          Left = 16
          Top = 118
          Width = 809
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Sign CSR File to CRT File '
          TabOrder = 4
          OnClick = btnSignClick
        end
        object edtRootCRT: TEdit
          Left = 88
          Top = 84
          Width = 641
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
        end
        object btnRootCRTBrowse: TButton
          Left = 736
          Top = 84
          Width = 91
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse CRT File'
          TabOrder = 6
          OnClick = btnRootCRTBrowseClick
        end
      end
      object grpParseCER: TGroupBox
        Left = 8
        Top = 168
        Width = 841
        Height = 359
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Parse CRT File'
        TabOrder = 1
        object lblCRT: TLabel
          Left = 16
          Top = 24
          Width = 44
          Height = 13
          Caption = 'CRT File:'
        end
        object edtCRT: TEdit
          Left = 88
          Top = 20
          Width = 377
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object btnBrowseCRT: TButton
          Left = 472
          Top = 20
          Width = 73
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse CRT'
          TabOrder = 1
          OnClick = btnBrowseCRTClick
        end
        object mmoCRT: TMemo
          Left = 16
          Top = 56
          Width = 809
          Height = 287
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 5
        end
        object btnParseCRT: TButton
          Left = 552
          Top = 20
          Width = 73
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Parse CRT'
          TabOrder = 2
          OnClick = btnParseCRTClick
        end
        object btnVerifySelfSignedCRT: TButton
          Left = 632
          Top = 20
          Width = 113
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Verify Self-Signed CRT'
          TabOrder = 3
          OnClick = btnVerifySelfSignedCRTClick
        end
        object btnVerifyCRT: TButton
          Left = 752
          Top = 20
          Width = 73
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Verify CRT'
          TabOrder = 4
          OnClick = btnVerifyCRTClick
        end
      end
    end
    object tsMisc: TTabSheet
      Caption = 'Misc'
      ImageIndex = 2
      object grpMisc: TGroupBox
        Left = 8
        Top = 8
        Width = 841
        Height = 521
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Misc'
        TabOrder = 0
        object btnGetWinRoot: TButton
          Left = 16
          Top = 24
          Width = 305
          Height = 25
          Caption = 'Get Windows Root Certs'
          TabOrder = 0
          OnClick = btnGetWinRootClick
        end
        object lstCerts: TListBox
          Left = 16
          Top = 64
          Width = 305
          Height = 361
          ItemHeight = 13
          TabOrder = 1
          OnClick = lstCertsClick
        end
        object mmoCertInfo: TMemo
          Left = 344
          Top = 64
          Width = 473
          Height = 361
          TabOrder = 2
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    Left = 532
    Top = 56
  end
  object dlgSave: TSaveDialog
    Left = 412
    Top = 152
  end
end
