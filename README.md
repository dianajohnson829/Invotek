# 📋 Invotek - Invoice Financing DAO

> 🚀 Trade unpaid invoices on-chain with decentralized finance

## 🌟 Overview

Invotek is a revolutionary decentralized autonomous organization (DAO) that enables businesses to trade unpaid invoices on the blockchain. Convert your outstanding receivables into immediate liquidity by selling them to investors at a discount.

## ✨ Features

- 📝 **Create Invoices**: Issue invoices on-chain with debtor details and due dates
- 💰 **Invoice Trading**: List invoices for sale at discounted prices
- 🤝 **Offer System**: Make and accept offers for invoice purchases
- 💳 **Direct Payment**: Debtors can pay invoices directly through the contract
- 📊 **User Statistics**: Track trading volume and activity
- 🏛️ **DAO Treasury**: Automated fee collection for platform sustainability

## 🛠️ Core Functions

### For Invoice Issuers
- `create-invoice` - Create a new invoice with debtor and payment details
- `list-invoice-for-sale` - Put your invoice up for sale at a discount
- `remove-invoice-from-sale` - Remove invoice from marketplace

### For Investors
- `buy-invoice` - Purchase invoices directly at listed price
- `make-offer` - Submit offers for invoices
- `accept-offer` - Accept investor offers (for invoice owners)

### For Debtors
- `pay-invoice` - Pay outstanding invoices directly

### Read-Only Functions
- `get-invoice` - Retrieve invoice details
- `get-user-stats` - View user trading statistics
- `get-dao-treasury` - Check DAO treasury balance
- `is-invoice-expired` - Check if invoice is past due date

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd invotek
clarinet check
```

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy --testnet
```

## 💡 Usage Examples

### Creating an Invoice
```clarity
(contract-call? .invotek create-invoice 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 u1000000 u1000 "Payment for services")
```

### Listing Invoice for Sale
```clarity
(contract-call? .invotek list-invoice-for-sale u1 u800000)
```

### Buying an Invoice
```clarity
(contract-call? .invotek buy-invoice u1)
```

## 💰 Fee Structure

- **DAO Fee**: 2.5% of transaction value (configurable by contract owner)
- **Fees Support**: Platform development and maintenance

z