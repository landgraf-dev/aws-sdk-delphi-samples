object NewJobForm: TNewJobForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'New Transcribe Job'
  ClientHeight = 391
  ClientWidth = 504
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
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 504
    Height = 20
    Align = alTop
    AutoSize = False
    Caption = 'Job Name'
    ExplicitWidth = 486
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 67
    Width = 504
    Height = 20
    Margins.Left = 0
    Margins.Top = 24
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    AutoSize = False
    Caption = 'Language Code'
    ExplicitTop = 52
    ExplicitWidth = 486
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 2
    Top = 179
    Width = 502
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
    Left = 0
    Top = 216
    Width = 504
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
    Left = 0
    Top = 283
    Width = 504
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
  object Label6: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 134
    Width = 504
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
  object JobNameEdit: TEdit
    Left = 0
    Top = 20
    Width = 504
    Height = 23
    Align = alTop
    TabOrder = 0
    OnChange = JobNameEditChange
  end
  object MediaUriEdit: TEdit
    Left = 0
    Top = 154
    Width = 504
    Height = 23
    Align = alTop
    TabOrder = 2
    OnChange = JobNameEditChange
  end
  object VocabularyFilterEdit: TComboBox
    Left = 0
    Top = 236
    Width = 504
    Height = 23
    Align = alTop
    Style = csDropDownList
    TabOrder = 3
    Items.Strings = (
      'None')
  end
  object ContentRedactionEdit: TComboBox
    Left = 0
    Top = 303
    Width = 504
    Height = 23
    Align = alTop
    Style = csDropDownList
    TabOrder = 4
    Items.Strings = (
      'No'
      'Yes (output redacted transcript only)'
      'Yes (output redacted and unredacted transcripts)')
  end
  object ConfirmButton: TBitBtn
    Left = 167
    Top = 360
    Width = 85
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 5
  end
  object CancelButton: TBitBtn
    Left = 255
    Top = 360
    Width = 85
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 6
  end
  object LanguageCodeEdit: TComboBox
    Left = 0
    Top = 87
    Width = 504
    Height = 23
    Align = alTop
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      'Auto-detect')
  end
end
