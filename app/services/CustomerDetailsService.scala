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

package services

import connectors.CustomerDetailsConnector
import javax.inject.{Inject, Singleton}
import models.core.ErrorModel
import models.customerInfo.CustomerDetailsModel
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CustomerDetailsService @Inject()(val customerDetailsConnector: CustomerDetailsConnector) {

  def getCustomerDetails(vrn: String)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Either[ErrorModel, CustomerDetailsModel]] =
    customerDetailsConnector.getCustomerDetails(vrn)
}
