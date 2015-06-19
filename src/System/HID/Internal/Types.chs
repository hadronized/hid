-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module System.HID.Internal.Types where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <hidapi.h>

type HIDDevice = {#type hid_device#}

data HIDDeviceInfo = HIDDeviceInfo {
    hidPath               :: CString
  , hidVendorID           :: CUShort
  , hidProductID          :: CUShort
  , hidSerialNumber       :: CWString
  , hidReleaseNumber      :: CUShort
  , hidManufacturerString :: CWString
  , hidProductString      :: CWString
  , hidUsagePage          :: CUShort
  , hidUsage              :: CUShort
  , hidInterfaceNumber    :: CInt
  , hidNext               :: Ptr HIDDeviceInfo
  } deriving (Eq,Show)

instance Storable HIDDeviceInfo where
  sizeOf _ = {#sizeof hid_device_info#}
  alignment _ = {#alignof hid_device_info#}
  peek p =
    HIDDeviceInfo
      <$> peekByteOff p {#offsetof hid_device_info->path#}
      <*> peekByteOff p {#offsetof hid_device_info->vendor_id#}
      <*> peekByteOff p {#offsetof hid_device_info->product_id#}
      <*> peekByteOff p {#offsetof hid_device_info->serial_number#}
      <*> peekByteOff p {#offsetof hid_device_info->release_number#}
      <*> peekByteOff p {#offsetof hid_device_info->manufacturer_string#}
      <*> peekByteOff p {#offsetof hid_device_info->product_string#}
      <*> peekByteOff p {#offsetof hid_device_info->usage_page#}
      <*> peekByteOff p {#offsetof hid_device_info->usage#}
      <*> peekByteOff p {#offsetof hid_device_info->interface_number#}
      <*> peekByteOff p {#offsetof hid_device_info->next#}
  poke p di = do
    pokeByteOff p {#offsetof hid_device_info->path#} $ hidPath di
    pokeByteOff p {#offsetof hid_device_info->vendor_id#} $ hidVendorID di
    pokeByteOff p {#offsetof hid_device_info->product_id#} $ hidProductID di
    pokeByteOff p {#offsetof hid_device_info->serial_number#} $ hidSerialNumber di
    pokeByteOff p {#offsetof hid_device_info->release_number#} $ hidReleaseNumber di
    pokeByteOff p {#offsetof hid_device_info->manufacturer_string#} $ hidManufacturerString di
    pokeByteOff p {#offsetof hid_device_info->product_string#} $ hidProductString di
    pokeByteOff p {#offsetof hid_device_info->usage_page#} $ hidUsagePage di
    pokeByteOff p {#offsetof hid_device_info->usage#} $ hidUsage di
    pokeByteOff p {#offsetof hid_device_info->interface_number#} $ hidInterfaceNumber di
    pokeByteOff p {#offsetof hid_device_info->next#} $ hidNext di
