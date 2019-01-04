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

package config

import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.crypto.PlainText
import uk.gov.hmrc.play.bootstrap.filters.frontend.crypto.SessionCookieCrypto
import uk.gov.hmrc.play.partials.HeaderCarrierForPartialsConverter

@Singleton
class VatcHeaderCarrierForPartialsConverter @Inject()(val sessionCookieCrypto: SessionCookieCrypto) extends HeaderCarrierForPartialsConverter {
  val crypto: String => String = cookie => sessionCookieCrypto.crypto.encrypt(PlainText(cookie)).value
}