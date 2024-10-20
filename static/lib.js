// Accessing the property via string literal prevents renaming by javascript minifiers which can cause FFI errors
window['skeleton_lib'] = {
  log: txt => console.log('Received "' + txt + '" from FFI'),
};

async function connect() {
  if (window.ethereum) {
     await window.ethereum.request({ method: "eth_requestAccounts" });
     window.web3 = new Web3(window.ethereum);
     const account = web3.eth.accounts;
     //Get the current MetaMask selected/active wallet
     const walletAddress = account.givenProvider.selectedAddress;
     console.log(`Wallet: ${walletAddress}`);
  
  } else {
   console.log("No wallet");
  }
}
