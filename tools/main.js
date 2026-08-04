import * as CSL from '@emurgo/cardano-serialization-lib-browser';

// HTLC compiled code from plutus.json - single CBOR encoded
const HTLC_SCRIPT_HEX = '5903d401010029800aba4aba2aba1aba0aab9faab9eaab9dab9cab9a4888888888c96600264653001300a00198051805800cdc3a4005300a0024888966002600460146ea800e2646644b300100789919912cc004c00c006264b300100180744c96600200300f807c03e01f13259800980c001c01602080a8dd7000a0303015001404c60226ea802a2b300130080018acc004c044dd5005400a01a809201a807100e0acc004c004c038dd50014660026024601e6ea800a46026602860286028602860286028602860280032232330010010032259800800c528456600266e3cdd7180b000801c528c4cc008008c05c005010202891809980a000c8c04cc050c050c050c050c050c050c0500052222259800980318099baa00d8992cc004cc0052410f505245494d414745204641494c4544005980099b8f37286eb8c060c054dd50071bae30183015375400d14a3153301349012b626c616b6532625f32353628707265696d61676529203d3d20646174756d2e68617368203f2046616c73650014a080922b3001330014910c56414c4944204245464f5245005980099191919912cc004c044c064dd50014566002602260326ea8c074c07800e266e20dd6980e980d1baa002001899b89375a603a60346ea80080050174528202e301b001375a600c60306ea8024cc064c068004cc0666002601c602c6ea8c068c06c00698103d87a8000a60103d8798000405097ae0301637546008602c6ea8004c008c054dd5005c528c54cc04d24013676616c69645f6265666f72652874782e76616c69646974795f72616e67652c20646174756d2e74696d656f757429203f2046616c73650014a080922660086eb0c014c054dd50059bae30183019301930193015375400d14a080922941012112cc00400629462a660280042c809a2b30013232323322598009808180c1baa0028acc004c040c060dd5180e180e801c4cdc40009bad301c301937540051337120026eb4c070c064dd5001202c8a50405860340026eb4c014c05cdd50041980c180c8009980c4c004c034c054dd5180c980d000d30103d87a8000a60103d8798000404c97ae0301537546030602a6ea8004c004c050dd500544cc00cdd61802180a1baa00a375c602e6030603060286ea801629410112022454cc0352411e65787065637420536f6d6528646174756d29203d20646174756d5f6f707400164030601c6ea8020dc3a400100a805402a0148098c03c004c03cc040004c02cdd5001c590080c028004c014dd5005c5268a99801a491856616c696461746f722072657475726e65642066616c7365001365640082a6600492011272656465656d65723a2052656465656d6572001601';

const BLOCKFROST_URLS = {
  Preview: 'https://cardano-preview.blockfrost.io/api/v0',
  Preprod: 'https://cardano-preprod.blockfrost.io/api/v0',
  Mainnet: 'https://cardano-mainnet.blockfrost.io/api/v0',
};

let walletApi = null;
let projectId = null;
let networkKey = null;

function hexToBytes(hex) {
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < hex.length; i += 2) {
    bytes[i / 2] = parseInt(hex.substr(i, 2), 16);
  }
  return bytes;
}

function bytesToHex(bytes) {
  return Array.from(bytes).map(b => b.toString(16).padStart(2, '0')).join('');
}

function setStatus(id, msg, type) {
  const el = document.getElementById(id);
  el.className = `status ${type}`;
  el.textContent = msg;
}

async function blockfrostGet(path) {
  const res = await fetch(`${BLOCKFROST_URLS[networkKey]}${path}`, {
    headers: { project_id: projectId },
  });
  if (!res.ok) throw new Error(`Blockfrost ${res.status}: ${await res.text()}`);
  return res.json();
}

async function blockfrostPost(path, body) {
  const res = await fetch(`${BLOCKFROST_URLS[networkKey]}${path}`, {
    method: 'POST',
    headers: { project_id: projectId, 'Content-Type': 'application/cbor' },
    body,
  });
  if (!res.ok) {
    const text = await res.text();
    throw new Error(`(${res.status})\n${text}`);
  }
  return res.json();
}

// Detect CIP-30 wallets
async function detectWallets() {
  const sel = document.getElementById('wallet-select');
  sel.innerHTML = '';
  const known = ['eternl', 'nami', 'flint', 'lace', 'yoroi', 'typhon', 'gerowallet', 'nufi'];
  const found = [];
  for (const name of known) {
    if (window.cardano && window.cardano[name]) found.push(name);
  }
  if (found.length === 0) {
    sel.innerHTML = '<option value="">No CIP-30 wallet detected</option>';
    return;
  }
  for (const name of found) {
    const opt = document.createElement('option');
    opt.value = name;
    opt.textContent = name.charAt(0).toUpperCase() + name.slice(1);
    sel.appendChild(opt);
  }
}

// Connect wallet
window.connectWallet = async function () {
  const walletName = document.getElementById('wallet-select').value;
  networkKey = document.getElementById('network-select').value;
  projectId = document.getElementById('blockfrost-key').value.trim();

  if (!walletName) return setStatus('wallet-status', 'No wallet selected', 'error');
  if (!projectId) return setStatus('wallet-status', 'Blockfrost project ID required', 'error');

  setStatus('wallet-status', 'Connecting...', 'info');

  try {
    walletApi = await window.cardano[walletName].enable();

    const addrsHex = await walletApi.getUsedAddresses();
    const addrHex = addrsHex[0] || (await walletApi.getUnusedAddresses())[0];
    const addr = CSL.Address.from_bytes(hexToBytes(addrHex)).to_bech32();

    const balanceHex = await walletApi.getBalance();
    const value = CSL.Value.from_bytes(hexToBytes(balanceHex));
    const lovelace = Number(value.coin().to_str()) / 1_000_000;

    setStatus('wallet-status',
      `Connected to ${walletName} on ${networkKey}\nAddress: ${addr}\nBalance: ${lovelace.toFixed(2)} ADA`,
      'success');

    document.getElementById('publish-btn').disabled = false;
    document.getElementById('send-btn').disabled = false;
  } catch (err) {
    setStatus('wallet-status', `Connection failed: ${err.message || err}`, 'error');
  }
};

// Publish HTLC reference script
window.publishScript = async function () {
  if (!walletApi) return setStatus('publish-status', 'Connect wallet first', 'error');

  const btn = document.getElementById('publish-btn');
  btn.disabled = true;
  setStatus('publish-status', 'Building transaction...', 'info');

  try {
    // Get wallet address
    const addrsHex = await walletApi.getUsedAddresses();
    const addrHex = addrsHex[0] || (await walletApi.getUnusedAddresses())[0];
    const addr = CSL.Address.from_bytes(hexToBytes(addrHex));

    // Get UTxOs from wallet
    const utxosHex = await walletApi.getUtxos();
    if (!utxosHex || utxosHex.length === 0) throw new Error('No UTxOs available');

    const utxos = CSL.TransactionUnspentOutputs.new();
    for (const hex of utxosHex) {
      utxos.add(CSL.TransactionUnspentOutput.from_bytes(hexToBytes(hex)));
    }

    // Build the PlutusScript
    const scriptBytes = hexToBytes(HTLC_SCRIPT_HEX);
    const plutusScript = CSL.PlutusScript.new_v3(scriptBytes);

    // Log the script hash for verification
    const scriptHash = plutusScript.hash();
    console.log('Script hash:', bytesToHex(scriptHash.to_bytes()));

    console.log('HTLC script hash:', bytesToHex(scriptHash.to_bytes()));

    // Build output with reference script at the script address (map format for Babbage+)
    const scriptRef = CSL.ScriptRef.new_plutus_script(plutusScript);
    const output = CSL.TransactionOutputBuilder.new()
      .with_address(addr)
      .with_script_ref(scriptRef)
      .next()
      .with_coin(CSL.BigNum.from_str('6000000'))
      .build();

    // Get protocol parameters
    setStatus('publish-status', 'Fetching protocol parameters...', 'info');
    const pp = await blockfrostGet('/epochs/latest/parameters');

    const txBuilderConfig = CSL.TransactionBuilderConfigBuilder.new()
      .fee_algo(CSL.LinearFee.new(
        CSL.BigNum.from_str(pp.min_fee_a.toString()),
        CSL.BigNum.from_str(pp.min_fee_b.toString()),
      ))
      .pool_deposit(CSL.BigNum.from_str(pp.pool_deposit))
      .key_deposit(CSL.BigNum.from_str(pp.key_deposit))
      .max_value_size(parseInt(pp.max_val_size))
      .max_tx_size(parseInt(pp.max_tx_size))
      .coins_per_utxo_byte(CSL.BigNum.from_str(pp.coins_per_utxo_size))
      .ref_script_coins_per_byte(CSL.UnitInterval.new(
        CSL.BigNum.from_str(pp.min_fee_ref_script_cost_per_byte.toString()),
        CSL.BigNum.from_str('1'),
      ))
      .build();

    const txBuilder = CSL.TransactionBuilder.new(txBuilderConfig);
    txBuilder.add_output(output);
    txBuilder.add_inputs_from(utxos, CSL.CoinSelectionStrategyCIP2.LargestFirstMultiAsset);
    txBuilder.add_change_if_needed(addr);

    const txBody = txBuilder.build();
    const tx = CSL.Transaction.new(txBody, CSL.TransactionWitnessSet.new());
    const txHex = bytesToHex(tx.to_bytes());

    setStatus('publish-status', 'Waiting for wallet signature...', 'info');
    const witnessHex = await walletApi.signTx(txHex, true);
    const witnesses = CSL.TransactionWitnessSet.from_bytes(hexToBytes(witnessHex));

    const signedTx = CSL.Transaction.new(txBody, witnesses);
    const signedTxBytes = signedTx.to_bytes();

    setStatus('publish-status', 'Submitting transaction via Blockfrost...', 'info');
    const txHash = await blockfrostPost('/tx/submit', signedTxBytes);

    setStatus('publish-status',
      `Reference script published!\n\nTx hash: ${txHash}\nOutput: ${txHash}#0\nHolder address: ${holderAddr.to_bech32()}\nScript hash: ${bytesToHex(scriptHash.to_bytes())}\n\nSave this for your Register page REFERENCE_UTXOS config.\nWait for confirmation before using.`,
      'success');
  } catch (err) {
    setStatus('publish-status', `Failed: ${err.message || err}`, 'error');
  } finally {
    btn.disabled = false;
  }
};

// Send custom transaction
window.sendTx = async function () {
  if (!walletApi) return setStatus('send-status', 'Connect wallet first', 'error');

  const recipient = document.getElementById('tx-recipient').value.trim();
  const lovelace = document.getElementById('tx-lovelace').value.trim();

  if (!recipient || !lovelace) return setStatus('send-status', 'Recipient and amount required', 'error');

  const btn = document.getElementById('send-btn');
  btn.disabled = true;
  setStatus('send-status', 'Building transaction...', 'info');

  try {
    const addrsHex = await walletApi.getUsedAddresses();
    const addrHex = addrsHex[0] || (await walletApi.getUnusedAddresses())[0];
    const changeAddr = CSL.Address.from_bytes(hexToBytes(addrHex));
    const toAddr = CSL.Address.from_bech32(recipient);

    const utxosHex = await walletApi.getUtxos();
    const utxos = CSL.TransactionUnspentOutputs.new();
    for (const hex of utxosHex) {
      utxos.add(CSL.TransactionUnspentOutput.from_bytes(hexToBytes(hex)));
    }

    const pp = await blockfrostGet('/epochs/latest/parameters');
    const txBuilderConfig = CSL.TransactionBuilderConfigBuilder.new()
      .fee_algo(CSL.LinearFee.new(
        CSL.BigNum.from_str(pp.min_fee_a.toString()),
        CSL.BigNum.from_str(pp.min_fee_b.toString()),
      ))
      .pool_deposit(CSL.BigNum.from_str(pp.pool_deposit))
      .key_deposit(CSL.BigNum.from_str(pp.key_deposit))
      .max_value_size(parseInt(pp.max_val_size))
      .max_tx_size(parseInt(pp.max_tx_size))
      .coins_per_utxo_byte(CSL.BigNum.from_str(pp.coins_per_utxo_size))
      .ref_script_coins_per_byte(CSL.UnitInterval.new(
        CSL.BigNum.from_str(pp.min_fee_ref_script_cost_per_byte.toString()),
        CSL.BigNum.from_str('1'),
      ))
      .build();

    const txBuilder = CSL.TransactionBuilder.new(txBuilderConfig);
    txBuilder.add_output(CSL.TransactionOutput.new(toAddr, CSL.Value.new(CSL.BigNum.from_str(lovelace))));
    txBuilder.add_inputs_from(utxos, CSL.CoinSelectionStrategyCIP2.LargestFirstMultiAsset);
    txBuilder.add_change_if_needed(changeAddr);

    const txBody = txBuilder.build();
    const tx = CSL.Transaction.new(txBody, CSL.TransactionWitnessSet.new());

    setStatus('send-status', 'Waiting for wallet signature...', 'info');
    const txHex = bytesToHex(tx.to_bytes());
    const witnessHex = await walletApi.signTx(txHex, true);
    const witnesses = CSL.TransactionWitnessSet.from_bytes(hexToBytes(witnessHex));

    const signedTx = CSL.Transaction.new(txBody, witnesses);
    const txHash = await walletApi.submitTx(bytesToHex(signedTx.to_bytes()));

    setStatus('send-status', `Transaction submitted!\nTx hash: ${txHash}`, 'success');
  } catch (err) {
    setStatus('send-status', `Failed: ${err.message || err}`, 'error');
  } finally {
    btn.disabled = false;
  }
};

// Init
detectWallets();
setTimeout(detectWallets, 1000);
