object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 391
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 501
    Height = 391
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Chat'
      DesignSize = (
        493
        363)
      object Button1: TButton
        Left = 2
        Top = 5
        Width = 161
        Height = 25
        Caption = 'Restart chat'
        TabOrder = 0
        OnClick = Button1Click
      end
      object mmChat: TMemo
        Left = 2
        Top = 36
        Width = 486
        Height = 298
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
      end
      object edMessage: TEdit
        Left = 2
        Top = 339
        Width = 421
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 2
      end
      object Send: TButton
        Left = 431
        Top = 339
        Width = 57
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = 'Send'
        Default = True
        TabOrder = 3
        OnClick = SendClick
      end
    end
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object mmLog: TMemo
        Left = 0
        Top = 0
        Width = 493
        Height = 363
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 104
        ExplicitTop = 200
        ExplicitWidth = 185
        ExplicitHeight = 89
      end
    end
  end
end
