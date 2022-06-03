object VocabularyFilterForm: TVocabularyFilterForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Vocabulary Filter'
  ClientHeight = 581
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 541
    Width = 484
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ConfirmButton: TBitBtn
      AlignWithMargins = True
      Left = 302
      Top = 4
      Width = 85
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Enabled = False
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
    end
    object CancelButton: TBitBtn
      AlignWithMargins = True
      Left = 395
      Top = 4
      Width = 85
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 476
    Height = 533
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = FilterDetailsTab
    Align = alClient
    TabOrder = 1
    object FilterDetailsTab: TTabSheet
      Caption = 'Filter Details'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 468
        Height = 503
        Margins.Left = 4
        Margins.Top = 8
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 20
        TabOrder = 0
        object Label1: TLabel
          Left = 20
          Top = 20
          Width = 428
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = 'Vocabulary Filter Name'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 486
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 20
          Top = 87
          Width = 428
          Height = 20
          Margins.Left = 0
          Margins.Top = 24
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          AutoSize = False
          Caption = 'Language Code'
          ExplicitLeft = 0
          ExplicitTop = 52
          ExplicitWidth = 486
        end
        object Label6: TLabel
          AlignWithMargins = True
          Left = 20
          Top = 154
          Width = 428
          Height = 20
          Margins.Left = 0
          Margins.Top = 24
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          AutoSize = False
          Caption = 'Words'
          ExplicitLeft = 5
          ExplicitTop = 70
          ExplicitWidth = 524
        end
        object FilterNameEdit: TEdit
          Left = 20
          Top = 40
          Width = 428
          Height = 23
          Align = alTop
          TabOrder = 0
          OnChange = OnEditChange
        end
        object LanguageCodeEdit: TComboBox
          Left = 20
          Top = 107
          Width = 428
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'Auto-detect')
        end
        object WordsMemo: TMemo
          Left = 20
          Top = 174
          Width = 428
          Height = 309
          Align = alClient
          TabOrder = 2
          OnChange = OnEditChange
        end
      end
    end
  end
end
