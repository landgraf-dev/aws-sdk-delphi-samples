object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = MemoLeft
  Caption = 'AWS Translate'
  ClientHeight = 350
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 210
    Top = 0
    Height = 350
    ExplicitLeft = 264
    ExplicitTop = 144
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 210
    Height = 350
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      210
      350)
    object cbLanguageLeft: TComboBox
      Left = 5
      Top = 5
      Width = 200
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object MemoLeft: TMemo
      Left = 5
      Top = 32
      Width = 201
      Height = 311
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'Hello, world!')
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 237
    Top = 0
    Width = 214
    Height = 350
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 274
    DesignSize = (
      214
      350)
    object cbLanguageRight: TComboBox
      Left = 5
      Top = 5
      Width = 200
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object MemoRight: TMemo
      Left = 5
      Top = 32
      Width = 202
      Height = 311
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 1
      ExplicitWidth = 211
    end
  end
  object Panel3: TPanel
    Left = 213
    Top = 0
    Width = 24
    Height = 350
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 32
      Width = 23
      Height = 22
      Caption = '->'
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 0
      Top = 60
      Width = 23
      Height = 22
      Caption = '<-'
      OnClick = SpeedButton2Click
    end
  end
end
