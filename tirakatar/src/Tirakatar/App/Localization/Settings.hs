{-# OPTIONS_GHC -Wno-orphans #-}
module Tirakatar.App.Localization.Settings(
    SettingsPageStrings(..)
  , NetSetupStrings(..)
  , DeleteWalletStrings(..)
  ) where

import Data.Time
import Tirakatar.Text
import Tirakatar.App.Language
import Tirakatar.App.Elements.Input.Class
import Tirakatar.App.Localization.Input
import Tirakatar.App.Localization.IP

data SettingsPageStrings =
    STPSTitle
  | STPSButLanguage
  | STPSButNetwork
  | STPSButPortfolio
  | STPSButMnemonicExport
  | STPSButDns
  | STPSButTor
  | STPSButSetPass
  | STPSButDeleteWallet
  | STPSSelectLanguage
  | STPSSetsTor
  | STPSUseTor
  | STPSSetsProxy
  | STPSProxyIpField
  | STPSProxyPortField
  | STPSSetsActiveCurrs
  | STPSSetsPinOn
  | STPSSetsPinOff
  | STPSSetsPinInput
  | STPSSetsPinDoSet
  | STPSMnemonicExportMsg
  | STPSSuccess
  | STPSIPStrings IPStrings
  | STPSInputStrings InputStrings
  deriving (Eq, Show)

instance Wrappable IPStrings SettingsPageStrings where
  wrap = STPSIPStrings

instance Wrappable InputStrings SettingsPageStrings where
  wrap = STPSInputStrings

instance LocalizedPrint SettingsPageStrings where
  localizedShow l v = case l of
    English -> case v of
      STPSTitle               -> "Settings"
      STPSButLanguage         -> "Language"
      STPSButNetwork          -> "Network"
      STPSButDns              -> "DNS servers"
      STPSButTor              -> "Tor and proxy"
      STPSButSetPass          -> "Change password"
      STPSButPortfolio        -> "Portfolio"
      STPSButMnemonicExport   -> "Export mnemonic phrase"
      STPSButDeleteWallet     -> "Delete wallet"
      STPSSelectLanguage      -> "Select language:"
      STPSSetsTor             -> "Tor configuration"
      STPSUseTor              -> "Use Tor"
      STPSSetsProxy           -> "SOCKS proxy configuration"
      STPSProxyIpField        -> "Proxy IP address"
      STPSProxyPortField      -> "Proxy port"
      STPSSetsActiveCurrs     -> "Settings for active currencies"
      STPSSetsPinOn           -> "Switch on PIN code"
      STPSSetsPinOff          -> "Switch off PIN code"
      STPSSetsPinInput        -> "Enter PIN code:"
      STPSSetsPinDoSet        -> "Set PIN code"
      STPSMnemonicExportMsg   -> "This is your password protected mnemonic phrase in QR code and text form. Choose the most convenient way."
      STPSSuccess             -> "Successfully updated settings"
      STPSIPStrings s         -> localizedShow English s
      STPSInputStrings s      -> localizedShow English s
    Russian -> case v of
      STPSTitle               -> "Настройки"
      STPSButLanguage         -> "Язык"
      STPSButNetwork          -> "Сеть"
      STPSButDns              -> "DNS сервера"
      STPSButTor              -> "Tor и прокси"
      STPSButSetPass          -> "Изменить пароль"
      STPSButPortfolio        -> "Портфель"
      STPSButMnemonicExport   -> "Экспортировать мнемоническую фразу"
      STPSButDeleteWallet     -> "Удалить кошелёк"
      STPSSelectLanguage      -> "Выберите язык:"
      STPSSetsTor             -> "Настройки Tor"
      STPSUseTor              -> "Проксировать через Tor"
      STPSSetsProxy           -> "Настройки прокси SOCKS"
      STPSProxyIpField        -> "Адрес прокси"
      STPSProxyPortField      -> "Порт прокси"
      STPSSetsActiveCurrs     -> "Настройки активных валют"
      STPSSetsPinOn           -> "Включить ПИН код"
      STPSSetsPinOff          -> "Выключить ПИН код"
      STPSSetsPinInput        -> "Введите ПИН код:"
      STPSSetsPinDoSet        -> "Установить ПИН код"
      STPSMnemonicExportMsg   -> "Это ваша защищенная паролем мнемоническая фраза в виде QR-кода и в текстовом виде. Используйте наиболее подходящий для вас способ."
      STPSSuccess             -> "Настройки успешно обновлены"
      STPSIPStrings s         -> localizedShow Russian s
      STPSInputStrings s      -> localizedShow Russian s

data NetSetupStrings
  = NSSFailedDns
  | NSSResolveConfDefault
  | NSSRestoreUrls
  | NSSSave
  | NSSAddDns
  | NSSCancel

instance LocalizedPrint NetSetupStrings where
  localizedShow l v = case l of
    English -> case v of
      NSSFailedDns    -> "Failed to parse DNS IP"
      NSSResolveConfDefault -> "Using servers from system configuration"
      NSSRestoreUrls  -> "Restore default"
      NSSSave         -> "Save"
      NSSAddDns       -> "Add DNS"
      NSSCancel       -> "Cancel"
    Russian -> case v of
      NSSFailedDns    -> "Некорректный IP DNS сервера"
      NSSResolveConfDefault -> "Используем глобальные настройки системы"
      NSSRestoreUrls  -> "Сервера по умолчанию"
      NSSSave         -> "Сохранить"
      NSSAddDns       -> "Добавить DNS"
      NSSCancel       -> "Отменить"

data DeleteWalletStrings
  = DWSTitle
  | DWSWarn1
  | DWSBtnYes
  | DWSBtnPass
  | DWSBtnNo
  | DWSWarn2
  | DWSWarn2Desc
  | DWSWarn3
  | DWSFinStage
  | DWSFin

instance LocalizedPrint DeleteWalletStrings where
  localizedShow l v = case l of
    English -> case v of
      DWSTitle      -> "Delete wallet"
      DWSBtnYes     -> "Delete"
      DWSBtnPass    -> "Enter the password"
      DWSBtnNo      -> "Cancel"
      DWSWarn1      -> "Are you sure you want to delete this wallet?"
      DWSWarn2      -> "Do you have your mnemonic?"
      DWSWarn2Desc  -> "If you don't, you will lose access to all your money. Forever."
      DWSWarn3      -> "One last step. Enter the password to continue."
      DWSFinStage   -> "Okay, you have convinced me. Go on."
      DWSFin        -> "The wallet has been deleted. Redirecting."
    Russian -> case v of
      DWSTitle      -> "Удаление кошелька"
      DWSWarn1      -> "Вы уверены, что хотите удалить этот кошелёк?"
      DWSBtnYes     -> "Удалить"
      DWSBtnPass    -> "Ввести пароль"
      DWSBtnNo      -> "Отмена"
      DWSWarn2      -> "Сохранили ли вы мнемоническую фразу?"
      DWSWarn2Desc  -> "Без неё вы потеряете доступ к своим деньгам. Навсегда."
      DWSWarn3      -> "Последняя проверка. Введите пароль для подтверждения."
      DWSFinStage   -> "Окей, вы убедили меня. Можете удалять."
      DWSFin        -> "Кошелёк успешно удалён. Перенаправляю."
