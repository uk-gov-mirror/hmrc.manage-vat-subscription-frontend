/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package connectors

import assets.BaseTestConstants.vrn
import assets.PaymentsTestConstants._
import connectors.httpParsers.ResponseHttpParser.HttpPostResult
import mocks.MockHttp
import models.core.ErrorModel
import models.payments.{NextUrl, PaymentRedirectModel, PaymentStartModel}
import play.api.http.HeaderNames.LOCATION
import play.api.http.Status
import uk.gov.hmrc.http.HttpResponse
import utils.TestUtil
import play.api.libs.json.Json

import scala.concurrent.Future

class PaymentsConnectorSpec extends TestUtil with MockHttp {

  val errorModel = HttpResponse(Status.BAD_REQUEST, responseString = Some("Error Message"))

  object TestPaymentsConnector extends PaymentsConnector(mockHttp,frontendAppConfig)

  "PaymentsConnector" when {

    val continueUrl = "continue-url"
    def postPaymentsDetailsResult: Future[HttpPostResult[PaymentRedirectModel]] =
      TestPaymentsConnector.postPaymentsDetails(PaymentStartModel("someVrn", true, "returnUrl", "backUrl"))

    "for postPaymentsDetails method" when {

      "when given a successful response" should {

        "return a Right with a PaymentRedirectModel" in {
          val successfulResponse = Right(PaymentRedirectModel(NextUrl(continueUrl)))
          setupMockHttpPost(s"${frontendAppConfig.bankAccountCoc}/bank-account-coc/start-journey-of-change-bank-account")(successfulResponse)
          await(postPaymentsDetailsResult) shouldBe Right(PaymentRedirectModel(NextUrl(continueUrl)))
        }
      }

      "given an unsuccessful response" should {

        "return an Left with an ErrorModel" in {
          val failedResponse = Left(ErrorModel(Status.INTERNAL_SERVER_ERROR, "Bad things"))
          setupMockHttpPost(s"${frontendAppConfig.bankAccountCoc}/bank-account-coc/start-journey-of-change-bank-account")(failedResponse)
          await(postPaymentsDetailsResult) shouldBe Left(ErrorModel(Status.INTERNAL_SERVER_ERROR, "Bad things"))
        }
      }
    }
  }

}
