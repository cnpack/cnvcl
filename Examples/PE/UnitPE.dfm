object FormPE: TFormPE
  Left = 340
  Top = 198
  Width = 967
  Height = 563
  Caption = 'PE File Test'
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
  object lblPEFile: TLabel
    Left = 16
    Top = 16
    Width = 36
    Height = 13
    Caption = 'PE File:'
  end
  object bvl1: TBevel
    Left = 520
    Top = 16
    Width = 49
    Height = 25
    Shape = bsLeftLine
  end
  object edtPEFile: TEdit
    Left = 64
    Top = 16
    Width = 281
    Height = 21
    TabOrder = 0
  end
  object btnParsePE: TButton
    Left = 536
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Parse Module'
    TabOrder = 1
    OnClick = btnParsePEClick
  end
  object cbbRunModule: TComboBox
    Left = 616
    Top = 16
    Width = 329
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object btnBrowse: TButton
    Left = 352
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object btnParsePEFile: TButton
    Left = 432
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Parse File'
    TabOrder = 4
    OnClick = btnParsePEFileClick
  end
  object pgcPE: TPageControl
    Left = 16
    Top = 48
    Width = 929
    Height = 473
    ActivePage = tsDosHeader
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    object tsDosHeader: TTabSheet
      Caption = 'Dos Header'
      object mmoDos: TMemo
        Left = 16
        Top = 16
        Width = 313
        Height = 409
        TabOrder = 0
      end
    end
    object tsNtHeader: TTabSheet
      Caption = 'NtHeader'
      ImageIndex = 1
      object mmoFile: TMemo
        Left = 16
        Top = 16
        Width = 313
        Height = 409
        TabOrder = 0
      end
      object mmoOptional: TMemo
        Left = 336
        Top = 16
        Width = 377
        Height = 409
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object tlb1: TToolBar
        Left = 784
        Top = 16
        Width = 137
        Height = 429
        Align = alNone
        ButtonHeight = 21
        ButtonWidth = 88
        Caption = 'tlb1'
        EdgeBorders = []
        ShowCaptions = True
        TabOrder = 2
        object btn0: TToolButton
          Left = 0
          Top = 2
          Caption = '0. Export'
          ImageIndex = 0
          Wrap = True
          OnClick = btn0Click
        end
        object btn1: TToolButton
          Tag = 1
          Left = 0
          Top = 23
          Caption = '1. Import'
          ImageIndex = 1
          Wrap = True
          OnClick = btn0Click
        end
        object btn2: TToolButton
          Tag = 2
          Left = 0
          Top = 44
          Caption = '2. Resource'
          ImageIndex = 2
          Wrap = True
          OnClick = btn0Click
        end
        object btn3: TToolButton
          Tag = 3
          Left = 0
          Top = 65
          Caption = '3. Exception'
          ImageIndex = 3
          Wrap = True
          OnClick = btn0Click
        end
        object btn4: TToolButton
          Tag = 4
          Left = 0
          Top = 86
          Caption = '4. Security'
          ImageIndex = 4
          Wrap = True
          OnClick = btn0Click
        end
        object btn5: TToolButton
          Tag = 5
          Left = 0
          Top = 107
          Caption = '5. Base Reloc'
          ImageIndex = 5
          Wrap = True
          OnClick = btn0Click
        end
        object btn6: TToolButton
          Tag = 6
          Left = 0
          Top = 128
          Caption = '6. Debug'
          ImageIndex = 6
          Wrap = True
          OnClick = btn0Click
        end
        object btn7: TToolButton
          Tag = 7
          Left = 0
          Top = 149
          Caption = '7. Copyright'
          ImageIndex = 7
          Wrap = True
          OnClick = btn0Click
        end
        object btn8: TToolButton
          Tag = 8
          Left = 0
          Top = 170
          Caption = '8. Global Ptr'
          ImageIndex = 8
          Wrap = True
          OnClick = btn0Click
        end
        object btn9: TToolButton
          Tag = 9
          Left = 0
          Top = 191
          Caption = '9. TLS'
          ImageIndex = 9
          Wrap = True
          OnClick = btn0Click
        end
        object btn10: TToolButton
          Tag = 10
          Left = 0
          Top = 212
          Caption = '10. Load Config'
          ImageIndex = 10
          Wrap = True
          OnClick = btn0Click
        end
        object btn11: TToolButton
          Tag = 11
          Left = 0
          Top = 233
          Caption = '11. Bound Import'
          ImageIndex = 11
          Wrap = True
          OnClick = btn0Click
        end
        object btn12: TToolButton
          Tag = 12
          Left = 0
          Top = 254
          Caption = '12. IAT'
          ImageIndex = 12
          Wrap = True
          OnClick = btn0Click
        end
        object btn13: TToolButton
          Tag = 13
          Left = 0
          Top = 275
          Caption = '13. Delay Import'
          ImageIndex = 13
          Wrap = True
          OnClick = btn0Click
        end
        object btn14: TToolButton
          Tag = 14
          Left = 0
          Top = 296
          Caption = '14. COM Desc'
          ImageIndex = 14
          Wrap = True
          OnClick = btn0Click
        end
        object btn15: TToolButton
          Tag = 15
          Left = 0
          Top = 317
          Caption = '15. ?'
          ImageIndex = 15
          OnClick = btn0Click
        end
      end
    end
    object tsSectionHeader: TTabSheet
      Caption = 'SectionHeader'
      ImageIndex = 2
      object mmoSection: TMemo
        Left = 16
        Top = 16
        Width = 313
        Height = 409
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object btnViewSection: TButton
        Left = 352
        Top = 16
        Width = 75
        Height = 25
        Caption = 'View Section'
        TabOrder = 1
        OnClick = btnViewSectionClick
      end
      object edtSectionIndex: TEdit
        Left = 440
        Top = 16
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '0'
      end
      object udSection: TUpDown
        Left = 481
        Top = 16
        Width = 15
        Height = 21
        Associate = edtSectionIndex
        Min = 0
        Position = 0
        TabOrder = 3
        Wrap = False
      end
    end
  end
  object dlgOpen1: TOpenDialog
    Left = 264
    Top = 32
  end
end
