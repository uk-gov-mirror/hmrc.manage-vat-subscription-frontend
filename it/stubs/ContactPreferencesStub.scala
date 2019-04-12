/*
 * Copyright 2019 HM Revenue & Customs
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

package stubs

import com.github.tomakehurst.wiremock.stubbing.StubMapping
import helpers.IntegrationTestConstants.VRN
import helpers.WireMockMethods
import models.circumstanceInfo.CircumstanceDetails
import models.core.SubscriptionUpdateResponseModel
import models.customerAddress.AddressLookupOnRampModel
import play.api.http.HeaderNames.LOCATION
import play.api.http.Status.{ACCEPTED, BAD_REQUEST, OK}
import play.api.libs.json.{JsObject, JsValue, Json}

object ContactPreferencesStub extends WireMockMethods {

  private val contactPrefUri: String => String = vrn => s"/contact-preferences/vat/vrn/$vrn"

  def getContactPrefs(status: Int, response: JsValue): StubMapping = {
    when(method = GET, uri = contactPrefUri(VRN))
      .thenReturn(status = status, body = response)
  }

}
