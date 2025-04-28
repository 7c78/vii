module Vii.InputMethodSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Data.Text qualified as T
import Vii.Test

spec :: Spec
spec = do
    describe "telex" $ do

        it "Kieu sample paragraphs" $ do
            paragraph telex $ T.unlines
                [ "trawm nawm trong coxi nguowfi ta"
                , "chuwx taif chuwx meejnh kheso laf ghest nhau"
                , "trari qua mootj cuoojc beer daau"
                , "nhuwxng ddieefu troong thaasy maf ddau ddowsn lofng"

                , "xanh um coor thuj tronf xoe las"
                , "trangws xoas trangf giang phawngr langjw towf"
                , "baauf docso giang sown say chapas ruouwj"
                , "tuis lungw phong nguyetje nangwj vif thow"

                , "chieuef troiwf bangr langr bongs hoangf hono"
                , "tiengse oocs xa dduaw laanx troongs doonf"
                , "gacs mais nguw oong veef vieenx xuws"
                , "khua suwngf mucj tuwr laij coo thoon"
                , "nganf mai gios cuoons chim bay moir"
                , "dawmj lieeux suowng sa khachs buowcs doonf"
                , "ker choons chuowng ddaif nguoiwf luwx thuws"
                , "laays ai maf keer nooix hanf oon"
                ]
            `shouldBe` T.unlines
                [ "trăm năm trong cõi người ta"
                , "chữ tài chữ mệnh khéo là ghét nhau"
                , "trải qua một cuộc bể dâu"
                , "những điều trông thấy mà đau đớn lòng"

                , "xanh um cổ thụ tròn xoe lá"
                , "trắng xoá tràng giang phẳng lặng tờ"
                , "bầu dốc giang sơn say chấp rượu"
                , "túi lưng phong nguyệt nặng vì thơ"

                , "chiều trời bảng lảng bóng hoàng hôn"
                , "tiếng ốc xa đưa lẫn trống dồn"
                , "gác mái ngư ông về viễn xứ"
                , "khua sừng mục tử lại cô thôn"
                , "ngàn mai gió cuốn chim bay mỏi"
                , "dặm liễu sương sa khách bước dồn"
                , "kẻ chốn chương đài người lữ thứ"
                , "lấy ai mà kể nỗi hàn ôn"
                ]
