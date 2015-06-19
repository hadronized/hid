-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module System.HID.Internal.Functions where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import System.HID.Internal.Types

#include <hidapi.h>

foreign import CALLCV "hid_init" hidInit :: IO CInt

foreign import CALLCV "hid_exit" hidExit :: IO CInt

foreign import CALLCV "hid_enumerate" hidEnumerate :: CUShort -> CUShort -> IO (Ptr HIDDeviceInfo)

foreign import CALLCV "hid_free_enumeration" hidFreeEnumeration :: Ptr HIDDeviceInfo -> IO ()

foreign import CALLCV "hid_open" hidOpen :: CUShort -> CUShort -> CWString -> IO (Ptr HIDDevice)

foreign import CALLCV "hid_open_path" hidOpenPath :: CString -> IO (Ptr HIDDevice)

foreign import CALLCV "hid_write" hidWrite :: Ptr HIDDevice -> Ptr CUChar -> CSize -> IO CInt

foreign import CALLCV "hid_read_timeout" hidReadTimeout :: Ptr HIDDevice -> Ptr CUChar -> CSize -> CInt -> IO CInt

foreign import CALLCV "hid_read" hidRead :: Ptr HIDDevice -> Ptr CUChar -> CSize -> IO CInt

foreign import CALLCV "hid_set_nonblocking" hidSetNonblocking :: Ptr HIDDevice -> CInt -> IO CInt

foreign import CALLCV "hid_send_feature_report" hidSendFeatureReport :: Ptr HIDDevice -> Ptr CUChar -> CSize -> IO CInt

foreign import CALLCV "hid_get_feature_report" hidGetFeatureReport :: Ptr HIDDevice -> Ptr CUChar -> CSize -> IO CInt

foreign import CALLCV "hid_close" hidClose :: Ptr HIDDevice -> IO ()

foreign import CALLCV "hid_get_manufacturer_string" hidGetManufacturerString :: Ptr HIDDevice -> CWString -> CSize -> IO CInt

foreign import CALLCV "hid_get_product_string" hidGetProductString :: Ptr HIDDevice -> CWString -> CSize -> IO CInt

foreign import CALLCV "hid_get_serial_number_string" hidGetSerialNumberString :: Ptr HIDDevice -> CWString -> CSize -> IO CInt

foreign import CALLCV "hid_get_indexed_string" hidGetIndexedString :: Ptr HIDDevice -> CInt -> CWString -> CSize -> IO CInt

foreign import CALLCV "hid_error" hidError ::Ptr HIDDevice -> IO CWString
