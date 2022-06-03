object TranscriptionJobForm: TTranscriptionJobForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Transcription Job'
  ClientHeight = 571
  ClientWidth = 624
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
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 616
    Height = 523
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = JobDetailsTab
    Align = alClient
    TabHeight = 28
    TabOrder = 0
    OnChange = PageControlChange
    object JobDetailsTab: TTabSheet
      Caption = 'Job Details'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 608
        Height = 485
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 20
        TabOrder = 0
        object Label1: TLabel
          Left = 20
          Top = 20
          Width = 568
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = 'Job Name'
          ExplicitTop = 6
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 20
          Top = 87
          Width = 568
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
          Width = 568
          Height = 20
          Margins.Left = 0
          Margins.Top = 24
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          AutoSize = False
          Caption = 'Media URI'
          ExplicitLeft = 5
          ExplicitTop = 70
          ExplicitWidth = 524
        end
        object Label3: TLabel
          AlignWithMargins = True
          Left = 22
          Top = 199
          Width = 566
          Height = 13
          Margins.Left = 2
          Margins.Top = 2
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 
            'Amazon S3 location. Supported formats are MP3, MP4, WAV, FLAC, O' +
            'GG, AMR and WEBM'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = [fsItalic]
          ParentFont = False
          ExplicitWidth = 423
        end
        object Label4: TLabel
          AlignWithMargins = True
          Left = 20
          Top = 236
          Width = 568
          Height = 20
          Margins.Left = 0
          Margins.Top = 24
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          AutoSize = False
          Caption = 'Vocabulary Filter'
          ExplicitLeft = -5
          ExplicitTop = 263
          ExplicitWidth = 524
        end
        object Label5: TLabel
          AlignWithMargins = True
          Left = 20
          Top = 303
          Width = 568
          Height = 20
          Margins.Left = 0
          Margins.Top = 24
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          AutoSize = False
          Caption = 'Content redaction'
          ExplicitLeft = -5
          ExplicitTop = 263
          ExplicitWidth = 524
        end
        object JobNameEdit: TEdit
          Left = 20
          Top = 40
          Width = 568
          Height = 23
          Align = alTop
          TabOrder = 0
          OnChange = OnEditChange
        end
        object LanguageCodeEdit: TComboBox
          Left = 20
          Top = 107
          Width = 568
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'Auto-detect')
        end
        object MediaUriEdit: TEdit
          Left = 20
          Top = 174
          Width = 568
          Height = 23
          Align = alTop
          TabOrder = 2
          OnChange = OnEditChange
        end
        object VocabularyFilterEdit: TComboBox
          Left = 20
          Top = 256
          Width = 568
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 3
          Items.Strings = (
            'None')
        end
        object ContentRedactionEdit: TComboBox
          Left = 20
          Top = 323
          Width = 568
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 4
          Items.Strings = (
            'No'
            'Yes (output redacted transcript only)'
            'Yes (output redacted and unredacted transcripts)')
        end
      end
    end
    object TranscriptTab: TTabSheet
      Caption = 'Transcript'
      ImageIndex = 2
      object TranscriptMemo: TMemo
        AlignWithMargins = True
        Left = 4
        Top = 62
        Width = 600
        Height = 419
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object ViewTranscriptOption: TRadioGroup
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 600
        Height = 50
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Raw'
          'Segmented')
        TabOrder = 1
        OnClick = ViewTranscriptOptionClick
      end
    end
    object RedactedTranscriptTab: TTabSheet
      Caption = 'Redacted Transcript'
      ImageIndex = 3
      object RedactedTranscriptMemo: TMemo
        AlignWithMargins = True
        Left = 4
        Top = 62
        Width = 600
        Height = 419
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object ViewRedactedTranscriptOption: TRadioGroup
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 600
        Height = 50
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alTop
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Raw'
          'Segmented')
        TabOrder = 1
        OnClick = ViewRedactedTranscriptOptionClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 531
    Width = 624
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ConfirmButton: TBitBtn
      AlignWithMargins = True
      Left = 442
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
      Left = 535
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
end
