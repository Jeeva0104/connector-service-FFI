use napi::bindgen_prelude::*;
use std::result::Result; // Shadow napi::Result with std::Result
use napi_derive::napi;
use serde::{Deserialize, Serialize};
use std::fmt::Debug;
use std::marker::PhantomData;

pub trait PaymentMethodDataTypes {
    type Inner: Debug + Clone + Serialize;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultCardData {
    pub card_number: i64,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct DefaultPCIHolder;

impl PaymentMethodDataTypes for DefaultPCIHolder {
    type Inner = DefaultCardData;
}

#[derive(Debug, Clone, Serialize)]
pub struct RawCardNumber<T: PaymentMethodDataTypes>(pub T::Inner);

impl<T: PaymentMethodDataTypes> RawCardNumber<T> {
    pub fn peek(&self) -> T::Inner {
        self.0.clone()
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Card<T: PaymentMethodDataTypes> {
    card_number: RawCardNumber<T>,
}

pub trait CardConversionHelper<T: PaymentMethodDataTypes> {
    fn convert_card_details(card: CardDetails) -> Result<Card<T>, String>;
}

impl CardConversionHelper<DefaultPCIHolder> for DefaultPCIHolder {
    fn convert_card_details(card: CardDetails) -> Result<Card<DefaultPCIHolder>, String> {
        Ok(Card {
            card_number: RawCardNumber::<DefaultPCIHolder>(DefaultCardData {
                card_number: card.card_number,
            }),
        })
    }
}

impl<T: PaymentMethodDataTypes + CardConversionHelper<T>> ForeignTryFrom<CardDetails> for Card<T> {
    type Error = String;
    fn foreign_try_from(card: CardDetails) -> Result<Self, Self::Error> {
        T::convert_card_details(card)
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum PaymentMethodData<T: PaymentMethodDataTypes> {
    Card(Card<T>),
}

impl<T: PaymentMethodDataTypes + CardConversionHelper<T>> ForeignTryFrom<PaymentMethodRequest>
    for PaymentMethodData<T>
{
    type Error = String;
    fn foreign_try_from(value: PaymentMethodRequest) -> Result<Self, Self::Error> {
        match value {
            PaymentMethodRequest::Card(card_details) => {
                let card = Card::<T>::foreign_try_from(card_details)?;
                Ok(PaymentMethodData::Card(card))
            }
        }
    }
}

pub trait ConnectorIntegrationAnyV2<Flow, ResourceCommonData, Req, Resp> {
    fn get_connector_integration_v2(
        &self,
    ) -> BoxedConnectorIntegrationV2<'_, Flow, ResourceCommonData, Req, Resp>;
}

pub trait ConnectorIntegrationV2<Flow, ResourceCommonData, Req, Resp>:
    ConnectorIntegrationAnyV2<Flow, ResourceCommonData, Req, Resp> + Debug
{
    fn get_url(&self);
    fn get_request_body(&self, req: &RouterDataV2<Flow, ResourceCommonData, Req, Resp>);
    fn build_request(&self, req: &RouterDataV2<Flow, ResourceCommonData, Req, Resp>);
}

impl<S, Flow, ResourceCommonData, Req, Resp>
    ConnectorIntegrationAnyV2<Flow, ResourceCommonData, Req, Resp> for S
where
    S: ConnectorIntegrationV2<Flow, ResourceCommonData, Req, Resp>,
{
    fn get_connector_integration_v2(
        &self,
    ) -> BoxedConnectorIntegrationV2<Flow, ResourceCommonData, Req, Resp> {
        Box::new(self)
    }
}

pub type BoxedConnectorIntegrationV2<'a, Flow, ResourceCommonData, Req, Resp> =
    Box<&'a (dyn ConnectorIntegrationV2<Flow, ResourceCommonData, Req, Resp>)>;

pub trait ConnectorServiceTrait<T: PaymentMethodDataTypes>: PaymentAuthorizeV2<T> {}

#[derive(Debug, Clone, Serialize)]
pub struct Authorize;

pub trait PaymentAuthorizeV2<T: PaymentMethodDataTypes>:
    ConnectorIntegrationV2<Authorize, PaymentFlowData, PaymentsAuthorizeData<T>, PaymentsResponseData>
{
}

pub trait ForeignTryFrom<F>: Sized {
    type Error;
    fn foreign_try_from(from: F) -> Result<Self, Self::Error>;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaymentFlowData {
    pub payment_id: String,
}

impl ForeignTryFrom<PaymentAuthorizeRequest> for PaymentFlowData {
    type Error = String;
    fn foreign_try_from(value: PaymentAuthorizeRequest) -> Result<Self, Self::Error> {
        Ok(PaymentFlowData {
            payment_id: value.payment_id,
        })
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum PaymentsResponseData {
    TransactionResponse { status_code: u16 },
}

#[derive(Debug, Clone, Serialize)]
pub struct PaymentsAuthorizeData<T: PaymentMethodDataTypes> {
    pub payment_method_data: PaymentMethodData<T>,
    pub confirm: bool,
    pub merchant_account: String,
    pub amount: i64,
}

impl<T: PaymentMethodDataTypes + CardConversionHelper<T>> ForeignTryFrom<PaymentAuthorizeRequest>
    for PaymentsAuthorizeData<T>
{
    type Error = String;
    fn foreign_try_from(value: PaymentAuthorizeRequest) -> Result<Self, Self::Error> {
        Ok(Self {
            payment_method_data: PaymentMethodData::<T>::foreign_try_from(
                value.payment_method.clone(),
            )?,
            confirm: true,
            merchant_account: value.merchant_account,
            amount: value.amount,
        })
    }
}

#[derive(Debug, Clone)]
pub struct RouterDataV2<Flow, ResourceCommonData, FlowSpecificRequest, FlowSpecificResponse> {
    pub flow: PhantomData<Flow>,
    pub resource_common_data: ResourceCommonData,
    pub request: FlowSpecificRequest,
    pub response: Result<FlowSpecificResponse, String>,
}

pub trait FlowTypes {
    type Flow;
    type FlowCommonData;
    type Request;
    type Response;
}

impl<F, FCD, Req, Res> FlowTypes for RouterDataV2<F, FCD, Req, Res> {
    type Flow = F;
    type FlowCommonData = FCD;
    type Request = Req;
    type Response = Res;
}

#[derive(Clone, Debug)]
struct Bridge<Req, Res>(pub PhantomData<(Req, Res)>);

trait BridgeRequestResponse: Debug {
    type RequestBody;
    type ResponseBody;
    type ConnectorInputData: FlowTypes;

    fn request_body(&self, rd: Self::ConnectorInputData) -> Result<Self::RequestBody, String>
    where
        Self::RequestBody: TryFrom<Self::ConnectorInputData, Error = String>,
    {
        Self::RequestBody::try_from(rd)
    }
}

pub struct AdyenRouterData<
    RD: FlowTypes,
    T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize + 'static,
> {
    pub connector: Adyen<T>,
    pub router_data: RD,
}

#[derive(Debug, Clone, Copy)]
pub struct Adyen<T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize + 'static> {
    authorize: &'static dyn BridgeRequestResponse<
        RequestBody = AdyenPaymentRequest<T>,
        ResponseBody = String,
        ConnectorInputData = AdyenRouterData<
            RouterDataV2<
                Authorize,
                PaymentFlowData,
                PaymentsAuthorizeData<T>,
                PaymentsResponseData,
            >,
            T,
        >,
    >,
}

impl<RD: FlowTypes, T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize> FlowTypes
    for AdyenRouterData<RD, T>
{
    type Flow = RD::Flow;
    type FlowCommonData = RD::FlowCommonData;
    type Request = RD::Request;
    type Response = RD::Response;
}

impl<F, FCD, Req, Resp> FlowTypes for &RouterDataV2<F, FCD, Req, Resp> {
    type Flow = F;
    type FlowCommonData = FCD;
    type Request = Req;
    type Response = Resp;
}

impl<T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize + 'static> BridgeRequestResponse
    for Bridge<AdyenPaymentRequest<T>, String>
{
    type RequestBody = AdyenPaymentRequest<T>;
    type ResponseBody = String;
    type ConnectorInputData = AdyenRouterData<
        RouterDataV2<Authorize, PaymentFlowData, PaymentsAuthorizeData<T>, PaymentsResponseData>,
        T,
    >;
}

impl<T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize + 'static> Adyen<T> {
    pub const fn new() -> &'static Self {
        &Self {
            authorize: &Bridge::<AdyenPaymentRequest<T>, String>(PhantomData),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AdyenCard<T: PaymentMethodDataTypes + Debug + Serialize> {
    number: RawCardNumber<T>,
    card_holder_name: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
#[serde(rename_all = "lowercase")]
pub enum AdyenPaymentMethod<T: PaymentMethodDataTypes + Debug + Serialize> {
    #[serde(rename = "scheme")]
    AdyenCard(Box<AdyenCard<T>>),
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum PaymentMethod<T: PaymentMethodDataTypes + Debug + Serialize> {
    AdyenPaymentMethod(Box<AdyenPaymentMethod<T>>),
}

#[derive(Debug, Clone, Serialize)]
pub struct AdyenPaymentRequest<T: PaymentMethodDataTypes + Debug + Serialize> {
    amount: i64,
    merchant_account: String,
    payment_method: Option<PaymentMethod<T>>,
}

impl<T: PaymentMethodDataTypes + Debug + Copy + Clone + 'static + Serialize>
    TryFrom<
        AdyenRouterData<
            RouterDataV2<
                Authorize,
                PaymentFlowData,
                PaymentsAuthorizeData<T>,
                PaymentsResponseData,
            >,
            T,
        >,
    > for AdyenPaymentRequest<T>
{
    type Error = String;
    fn try_from(
        item: AdyenRouterData<
            RouterDataV2<
                Authorize,
                PaymentFlowData,
                PaymentsAuthorizeData<T>,
                PaymentsResponseData,
            >,
            T,
        >,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            amount: item.router_data.request.amount,
            merchant_account: item.router_data.request.merchant_account,
            payment_method: None,
        })
    }
}

impl<T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize> ConnectorServiceTrait<T>
    for Adyen<T>
{
}

impl<T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize> PaymentAuthorizeV2<T>
    for Adyen<T>
{
}

impl<T: PaymentMethodDataTypes + Debug + Clone + Copy + Serialize>
    ConnectorIntegrationV2<
        Authorize,
        PaymentFlowData,
        PaymentsAuthorizeData<T>,
        PaymentsResponseData,
    > for Adyen<T>
{
    fn get_url(&self) {}
    fn get_request_body(
        &self,
        req: &RouterDataV2<
            Authorize,
            PaymentFlowData,
            PaymentsAuthorizeData<T>,
            PaymentsResponseData,
        >,
    ) {
        let bridge = &self.authorize;
        let input_data = AdyenRouterData {
            connector: self.to_owned(),
            router_data: req.clone(),
        };
        let _request: Result<AdyenPaymentRequest<T>, String> = bridge.request_body(input_data);
    }
    fn build_request(
        &self,
        req: &RouterDataV2<
            Authorize,
            PaymentFlowData,
            PaymentsAuthorizeData<T>,
            PaymentsResponseData,
        >,
    ) {
        self.get_request_body(req);
    }
}

pub type BoxedConnector<T> = Box<dyn ConnectorServiceTrait<T>>;

#[derive(Debug)]
pub enum ConnectorEnum {
    Adyen,
}

#[derive(Debug)]
pub struct ConnectorData<T: PaymentMethodDataTypes + 'static> {
    pub connector: BoxedConnector<T>,
    pub connector_name: ConnectorEnum,
}

impl<T: PaymentMethodDataTypes + Debug + Serialize + Clone + Copy + 'static> ConnectorData<T> {
    pub fn get_connector_by_name(connector_name: ConnectorEnum) -> Self {
        Self {
            connector: Box::new(*Adyen::new()),
            connector_name,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CardDetails {
    pub card_number: i64,
    pub card_cvc: i64,
    pub card_issuer: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub enum PaymentMethodRequest {
    Card(CardDetails),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PaymentAuthorizeRequest {
    pub amount: i64,
    pub minor_amount: i64,
    pub payment_id: String,
    pub confirm: bool,
    pub merchant_account: String,
    pub payment_method: PaymentMethodRequest,
}

fn build_adyen_request<T: PaymentMethodDataTypes + CardConversionHelper<T> + Debug + Clone + Copy + Serialize + 'static>(
    grpc_request: PaymentAuthorizeRequest,
) -> Result<AdyenPaymentRequest<T>, String> {
    let payment_flow_data = PaymentFlowData::foreign_try_from(grpc_request.clone())?;
    let payment_authorize_data =
        PaymentsAuthorizeData::<T>::foreign_try_from(grpc_request.clone())?;

    let router_data = RouterDataV2::<
        Authorize,
        PaymentFlowData,
        PaymentsAuthorizeData<T>,
        PaymentsResponseData,
    > {
        flow: PhantomData,
        resource_common_data: payment_flow_data,
        request: payment_authorize_data,
        response: Err("response".to_string()),
    };

    let input_data = AdyenRouterData {
        connector: *Adyen::new(),
        router_data,
    };

    AdyenPaymentRequest::try_from(input_data)
}

pub fn authorize_json(request_json: String) -> Result<String, String> {
    let grpc_request: PaymentAuthorizeRequest =
        serde_json::from_str(&request_json).map_err(|e| e.to_string())?;
    let adyen_request = build_adyen_request::<DefaultPCIHolder>(grpc_request)?;
    serde_json::to_string(&adyen_request).map_err(|e| e.to_string())
}

#[napi]
pub fn authorize(request_json: String) -> napi::Result<String> {
    authorize_json(request_json).map_err(Error::from_reason)
}
